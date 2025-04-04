package io.github.humbleui.skija.examples;

import io.github.humbleui.skija.*;
import io.github.humbleui.skija.examples.scenes.HUD;
import io.github.humbleui.skija.examples.scenes.Pair;
import io.github.humbleui.skija.examples.scenes.Scenes;
import io.github.humbleui.skija.impl.Library;
import io.github.humbleui.skija.impl.Stats;
import io.github.humbleui.types.IRect;
import org.lwjgl.opengl.GL;

import java.lang.reflect.InvocationTargetException;

import static org.lwjgl.glfw.Callbacks.glfwFreeCallbacks;
import static org.lwjgl.glfw.GLFW.*;
import static org.lwjgl.system.MemoryUtil.NULL;

public class Window {
    public long window;
    public int width;
    public int height;
    public float dpi = 1f;
    public int xpos = 0;
    public int ypos = 0;
    public boolean vsync = true;
    private static final String os = System.getProperty("os.name").toLowerCase();

    public void run(IRect bounds) {
        createWindow(bounds);
        loop();

        glfwFreeCallbacks(window);
        glfwDestroyWindow(window);
        glfwTerminate();
        glfwSetErrorCallback(null).free();
    }

    private void updateDimensions() {
        int[] width = new int[1];
        int[] height = new int[1];
        glfwGetFramebufferSize(window, width, height);

        float[] xscale = new float[1];
        float[] yscale = new float[1];
        glfwGetWindowContentScale(window, xscale, yscale);
        assert xscale[0] == yscale[0] : "Horizontal dpi=" + xscale[0] + ", vertical dpi=" + yscale[0];

        this.width = (int) (width[0] / xscale[0]);
        this.height = (int) (height[0] / yscale[0]);
        this.dpi = xscale[0];
        System.out.println("FramebufferSize " + width[0] + "x" + height[0] + ", scale " + this.dpi + ", window " + this.width + "x" + this.height);
    }

    private void createWindow(IRect bounds) {
        glfwDefaultWindowHints(); // optional, the current window hints are already the default
        glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE); // the window will stay hidden after creation
        glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE); // the window will be resizable

        window = glfwCreateWindow(bounds.getWidth(), bounds.getHeight(), "Skija LWJGL Demo", NULL, NULL);
        if (window == NULL)
            throw new RuntimeException("Failed to create the GLFW window");

        glfwSetKeyCallback(window, (window, key, scancode, action, mods) -> {
            if (key == GLFW_KEY_ESCAPE && action == GLFW_RELEASE)
                glfwSetWindowShouldClose(window, true);
        });

        glfwSetWindowPos(window, bounds.getLeft(), bounds.getTop());
        updateDimensions();
        xpos = width / 2;
        ypos = height / 2;

        glfwMakeContextCurrent(window);
        glfwSwapInterval(vsync ? 1 : 0); // Enable v-sync
        glfwShowWindow(window);
    }

    private DirectContext context;
    private BackendRenderTarget renderTarget;
    private Surface surface;
    private Canvas canvas;

    private void initSkia() {
        Stats.enabled = true;

        if (surface != null)
            surface.close();
        if (renderTarget != null)
            renderTarget.close();

        renderTarget = BackendRenderTarget.makeGL(
                (int) (width * dpi),
                (int) (height * dpi),
                /*samples*/0,
                /*stencil*/8,
                /*fbId*/0,
                FramebufferFormat.GR_GL_RGBA8);

        surface = Surface.wrapBackendRenderTarget(
                context,
                renderTarget,
                SurfaceOrigin.BOTTOM_LEFT,
                SurfaceColorFormat.RGBA_8888,
                ColorSpace.getDisplayP3(),  // TODO load monitor profile
                new SurfaceProps(PixelGeometry.RGB_H));

        canvas = surface.getCanvas();
    }

    private void draw() {
        Scenes.draw(canvas, width, height, dpi, xpos, ypos);
        context.flush();
        glfwSwapBuffers(window);
    }

    private void loop() {
        GL.createCapabilities();
        if ("false".equals(System.getProperty("skija.staticLoad")))
            Library.load();
        context = DirectContext.makeGL();

        org.lwjgl.glfw.GLFW.glfwSetWindowSizeCallback(window, (window, width, height) -> {
            updateDimensions();
            initSkia();
            draw();
        });

        glfwSetCursorPosCallback(window, (window, xpos, ypos) -> {
            if(os.contains("mac") || os.contains("darwin")) {
                this.xpos = (int) xpos;
                this.ypos = (int) ypos;
            } else {
                this.xpos = (int) (xpos / dpi);
                this.ypos = (int) (ypos / dpi);
            }
        });

        glfwSetMouseButtonCallback(window, (window, button, action, mods) -> {
            // System.out.println("Button " + button + " " + (action == 0 ? "released" : "pressed"));
        });

        glfwSetScrollCallback(window, (window, xoffset, yoffset) -> {
            Scenes.currentScene().onScroll((float) xoffset * dpi, (float) yoffset * dpi);
        });

        glfwSetKeyCallback(window, (window, key, scancode, action, mods) -> {
            if (action == GLFW_PRESS) {
                switch (key) {
                    case GLFW_KEY_LEFT:
                        try {
                            Scenes.prevScene();
                        } catch (ClassNotFoundException | InvocationTargetException | NoSuchMethodException |
                                 InstantiationException | IllegalAccessException e) {
                            throw new RuntimeException(e);
                        }
                        break;
                    case GLFW_KEY_RIGHT:
                        try {
                            Scenes.nextScene();
                        } catch (ClassNotFoundException | InvocationTargetException | NoSuchMethodException |
                                 InstantiationException | IllegalAccessException e) {
                            throw new RuntimeException(e);
                        }
                        break;
                    case GLFW_KEY_UP:
                        Scenes.currentScene().changeVariant(-1);
                        break;
                    case GLFW_KEY_DOWN:
                        Scenes.currentScene().changeVariant(1);
                        break;
                    case GLFW_KEY_V:
                        vsync = !vsync;
                        glfwSwapInterval(vsync ? 1 : 0);
                        HUD.extras.set(0, new Pair("V", "VSync: " + (vsync ? "ON" : "OFF")));
                        break;
                    case GLFW_KEY_S:
                        Scenes.stats = !Scenes.stats;
                        Stats.enabled = Scenes.stats;
                        break;
                    case GLFW_KEY_G:
                        System.out.println("Before GC " + Stats.allocated);
                        System.gc();
                        break;
                }
            }
        });

        HUD.extras.add(new Pair("V", "VSync: " + (vsync ? "ON" : "OFF")));
        initSkia();

        while (!glfwWindowShouldClose(window)) {
            draw();
            glfwPollEvents();
        }
    }
}
