package io.github.humbleui.skija.examples.scenes;

import io.github.humbleui.skija.Canvas;

import java.lang.reflect.InvocationTargetException;
import java.util.Comparator;
import java.util.Optional;
import java.util.TreeMap;

public class Scenes {
    public static TreeMap<String, Scene> scenes;
    public static String currentScene = "Bitmap Image";
    public static HUD hud = new HUD();
    public static boolean stats = true;

    static {
        scenes = new TreeMap<>(Comparator.comparing(s -> s.toLowerCase()));
        scenes.put("Backdrop", null);
        scenes.put("Bitmap", null);
        scenes.put("Bitmap Image", null);
        scenes.put("Blends", null);
        scenes.put("Break Iterator", null);
        scenes.put("Codec", null);
        scenes.put("Color Filters", null);
        scenes.put("Decorations Bench", null);
        scenes.put("Drawable", null);
        scenes.put("Empty", null);
        scenes.put("Figma", null);
        scenes.put("Font", null);
        scenes.put("Font Rendering", null);
        scenes.put("Font Size", null);
        scenes.put("Font Variations", null);
        scenes.put("Geometry", null);
        scenes.put("Images", null);
        scenes.put("Image Bench", null);
        scenes.put("Image Codecs", null);
        scenes.put("Image Filters", null);
        scenes.put("Mask Filters", null);
        scenes.put("Matrix", null);
        scenes.put("Paragraph", null);
        scenes.put("Paragraph Metrics", null);
        scenes.put("Paragraph Style", null);
        scenes.put("Path Effects", null);
        scenes.put("Paths", null);
        scenes.put("Picture Recorder", null);
        scenes.put("Pixel Grid", null);
        scenes.put("Pythagoras", null);
        scenes.put("Run Handler", null);
        scenes.put("Run Iterator", null);
        scenes.put("Runtime Effect", null);
        scenes.put("SVG", null);
        scenes.put("SVG Scaling", null);
        scenes.put("Shaders", null);
        scenes.put("Shadows", null);
        scenes.put("Shadow Utils", null);
        scenes.put("Shapers", null);
        scenes.put("Skottie", null);
        scenes.put("Squares", null);
        scenes.put("Squircle", null);
        scenes.put("Swing", null);
        scenes.put("Text Shape Bench", null);
        scenes.put("Text Style", null);
        scenes.put("Text Blob", null);
        scenes.put("Text Line", null);
        scenes.put("Text Line Decorations", null);
        scenes.put("Text Line Locale", null);
        scenes.put("Wall Of Text", null);
        scenes.put("Watches", null);
        try {
            setScene(currentScene);
        } catch (ClassNotFoundException | InvocationTargetException | NoSuchMethodException | InstantiationException |
                 IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    public static Scene newScene(String name) throws NoSuchMethodException, InvocationTargetException, InstantiationException, IllegalAccessException, ClassNotFoundException {
        String className = "io.github.humbleui.skija.examples.scenes." + name.replaceAll(" ", "") + "Scene";
        Class<Scene> cls = (Class<Scene>) Class.forName(className);
        return cls.getDeclaredConstructor().newInstance();
    }

    public static Scene nextScene() throws ClassNotFoundException, InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {
        return setScene(Optional.ofNullable(scenes.higherKey(currentScene)).orElse(scenes.firstKey()));
    }

    public static Scene prevScene() throws ClassNotFoundException, InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {
        return setScene(Optional.ofNullable(scenes.lowerKey(currentScene)).orElse(scenes.lastKey()));
    }

    public static Scene setScene(String scene) throws ClassNotFoundException, InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {
        currentScene = scene;
        if (!scenes.containsKey(scene))
            throw new IllegalArgumentException("Unknown scene: " + scene);
        if (scenes.get(scene) == null)
            scenes.put(scene, newScene(scene));
        return scenes.get(scene);
    }

    public static Scene currentScene() {
        return scenes.get(currentScene);
    }

    public static void draw(Canvas canvas, int width, int height, float scale, int mouseX, int mouseY) {
        canvas.clear(0xFFFFFFFF);
        int layer = canvas.save();
        var scene = currentScene();
        if (scene.scale())
            canvas.scale(scale, scale);
        scene.draw(canvas, width, height, scale, mouseX, mouseY);
        canvas.restoreToCount(layer);

        hud.tick();
        if (stats) {
            layer = canvas.save();
            canvas.scale(scale, scale);
            hud.draw(canvas, scene, width, height);
            canvas.restoreToCount(layer);
        } else
            hud.log();
    }
}
