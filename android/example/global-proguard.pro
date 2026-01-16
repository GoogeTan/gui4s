# 1. SILENCE LOGS: Ignore "Info/Note" messages about malformed attributes
-dontnote **

# 2. SILENCE WARNINGS: Ignore warnings about stack map tables (dangerous but necessary here)
-ignorewarnings

# 3. CRITICAL FIXES FOR R8 CRASH (NPE)
# Disable optimization (R8 trying to rewrite code)
-dontoptimize
# Disable shrinking (R8 trying to remove code - likely hitting the invalid bytecode here)
-dontshrink
# Disable obfuscation (renaming) to ensure maximum compatibility during debug
-dontobfuscate

# 4. Keep attributes required for Scala 3
-keepattributes Signature,InnerClasses,EnclosingMethod,SourceFile,LineNumberTable,*Annotation*

# --- Library Specific Rules ---
-dontwarn scala.**
-dontwarn sun.misc.Unsafe
-dontwarn java.lang.System$Logger
-dontwarn java.lang.System$Logger$Level
-dontwarn java.lang.management.**
-dontwarn javax.management.**
-dontwarn java.net.UnixDomainSocketAddress
-dontwarn jnr.unixsocket.**
-dontwarn org.slf4j.impl.**
-dontwarn org.typelevel.log4cats.**
-dontwarn javax.lang.model.element.**
-dontwarn com.google.errorprone.annotations.**