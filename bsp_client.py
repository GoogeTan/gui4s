#!/usr/bin/env python3
import json
import subprocess
import sys
import os
import glob
from urllib.parse import urlparse, unquote

def send_message(proc, msg):
    """Sends a JSON-RPC message with the required Content-Length header."""
    body = json.dumps(msg).encode('utf-8')
    header = f"Content-Length: {len(body)}\r\n\r\n".encode('utf-8')
    proc.stdin.write(header + body)
    proc.stdin.flush()

def read_message(proc):
    """Reads the next JSON-RPC message from the server's stdout."""
    content_length = 0
    while True:
        line = proc.stdout.readline()
        if not line:
            return None
        line = line.decode('utf-8')
        if line == "\r\n":
            break
        if line.lower().startswith("content-length:"):
            content_length = int(line.split(":")[1].strip())

    if content_length == 0:
        return None

    body = proc.stdout.read(content_length).decode('utf-8')
    return json.loads(body)

def wait_for_response(proc, expected_id):
    """Waits for a specific JSON-RPC response ID, ignoring background notifications."""
    while True:
        msg = read_message(proc)
        if msg is None:
            print("Error: Server closed the connection unexpectedly.")
            sys.exit(1)

        if "id" in msg and msg["id"] == expected_id:
            if "error" in msg:
                print(f"Error from server: {msg['error']}")
                sys.exit(1)
            return msg.get("result", {})

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 bsp_client.py <module_name>")
        print("Example: python3 bsp_client.py android.skia")
        sys.exit(1)

    target_module = sys.argv[1]

    # 1. Find the BSP connection configuration
    bsp_configs = glob.glob('.bsp/*.json')
    if not bsp_configs:
        print("No BSP configuration found. Please run 'mill mill.bsp.BSP/install' first.")
        sys.exit(1)

    with open(bsp_configs[0], 'r') as f:
        config = json.load(f)

    argv = config.get("argv")
    if not argv:
        print(f"Invalid BSP config in {bsp_configs[0]}")
        sys.exit(1)

    print(f"Starting BSP server using: {' '.join(argv)}\n")

    # Start the server (stderr maps to sys.stderr so we can see Mill's background logs)
    proc = subprocess.Popen(argv, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=sys.stderr)

    root_uri = f"file://{os.path.abspath(os.getcwd())}"

    # 2. Initialize handshake
    send_message(proc, {
        "jsonrpc": "2.0", "id": 1, "method": "build/initialize",
        "params": {
            "displayName": "Python BSP Debugger", "version": "1.0",
            "bspVersion": "2.1.0", "rootUri": root_uri,
            "capabilities": {"languageIds": ["scala", "java"]}
        }
    })
    wait_for_response(proc, 1)

    send_message(proc, {
        "jsonrpc": "2.0", "method": "build/initialized", "params": {}
    })

    # 3. Ask for all build targets
    print("Fetching workspace build targets...")
    send_message(proc, {
        "jsonrpc": "2.0", "id": 2, "method": "workspace/buildTargets", "params": {}
    })
    targets_result = wait_for_response(proc, 2)

    # 4. Find our specific module
    target_uri = None
    available_targets = []

    for target in targets_result.get("targets", []):
        display_name = target.get("displayName", "")
        uri = target.get("id", {}).get("uri", "")
        available_targets.append(display_name)

        # Mill usually sets displayName directly to the module name
        if display_name == target_module or target_module in uri:
            target_uri = uri
            break

    if not target_uri:
        print(f"\nCould not find target matching '{target_module}'.")
        print("Available targets in this workspace:")
        for t in sorted(available_targets):
            print(f"  - {t}")
        sys.exit(1)

    print(f"Found target URI: {target_uri}\n")

    # 5. Ask for the compile classpath via scalacOptions
    print(f"Requesting scalacOptions for {target_module}...\n")
    send_message(proc, {
        "jsonrpc": "2.0", "id": 3, "method": "buildTarget/scalacOptions",
        "params": {"targets": [{"uri": target_uri}]}
    })
    cp_result = wait_for_response(proc, 3)

    # 6. Parse and print the results
    items = cp_result.get("items", [])
    if not items:
        print("No classpath items returned.")
    else:
        print("="*60)
        print("RAW CLASSPATH URIS RECEIVED VIA BSP:")
        print("="*60)
        for item in items:
            for classpath_uri in item.get("classpath", []):
                # We unquote to make paths readable (e.g., %20 to space)
                clean_path = unquote(urlparse(classpath_uri).path)

                # Highlight AAR files so the bug is glaringly obvious
                if clean_path.endswith('.aar'):
                    print(f"🚨 AAR DETECTED 🚨 : {clean_path}")
                else:
                    print(f"   {clean_path}")

    # Clean shutdown
    send_message(proc, {"jsonrpc": "2.0", "id": 4, "method": "build/shutdown"})
    wait_for_response(proc, 4)
    send_message(proc, {"jsonrpc": "2.0", "method": "build/exit"})
    proc.stdin.close()
    proc.wait()

if __name__ == "__main__":
    main()