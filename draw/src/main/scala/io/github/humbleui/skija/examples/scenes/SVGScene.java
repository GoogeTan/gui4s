package io.github.humbleui.skija.examples.scenes;

import io.github.humbleui.skija.Canvas;
import io.github.humbleui.skija.Paint;
import io.github.humbleui.skija.svg.SVGDOM;
import io.github.humbleui.skija.svg.SVGLengthContext;
import io.github.humbleui.skija.svg.SVGLengthType;
import io.github.humbleui.skija.svg.SVGSVG;
import io.github.humbleui.types.Point;
import io.github.humbleui.types.Rect;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;

public class SVGScene extends Scene {
    public Thread _thread = null;
    public SVGDOM _dom = null;
    public Throwable _error = null;

    public SVGScene() throws IOException {
        _variants = Files.lines(java.nio.file.Path.of(file("images/svgs.txt"))).toArray(String[]::new);
        _variantIdx = 3;
    }

    @Override
    public void draw(Canvas canvas, int width, int height, float dpi, int xpos, int ypos) {
        if (_dom != null) {
            try (SVGSVG root = _dom.getRoot();
                 var paint = new Paint().setColor(0xFFEEEEEE);)
            {
                var bounds = new Point(width - 50, height - 50);
                SVGLengthContext lc = new SVGLengthContext(bounds);
                var svgWidth = lc.resolve(root.getWidth(), SVGLengthType.HORIZONTAL);
                var svgHeight = lc.resolve(root.getHeight(), SVGLengthType.VERTICAL);
                _dom.setContainerSize(bounds);
                var scale = Math.min(bounds.getX() / svgWidth, bounds.getY() / svgHeight);
                canvas.translate((width - svgWidth * scale) / 2f, (height - svgHeight * scale) / 2f);
                canvas.drawRect(Rect.makeWH(svgWidth * scale, svgHeight * scale), paint);
                canvas.scale(scale, scale);
                _dom.render(canvas);
            }
        } else if (_error != null) {
            Scene.drawStringCentered(canvas, _error.getMessage(), width / 2, height / 2, Scene.inter13, Scene.blackFill);
        } else if (_thread != null) {
            Scene.drawStringCentered(canvas, "Loading " + _variants[_variantIdx] + "...", width / 2, height / 2, Scene.inter13, Scene.blackFill);
        } else {
            _thread = new Fetch(_variants[_variantIdx]);
            _thread.start();
        }
    }

    class Fetch extends Thread {
        public String _url;

        public Fetch(String url) {
            this._url = url;
        }

        @Override
        public void run() {
            try (var in = new BufferedInputStream(new URL(_url).openStream());
                 var out = new ByteArrayOutputStream();)
            {
                in.transferTo(out);
                if (_thread == this) {
                    try (var data = io.github.humbleui.skija.Data.makeFromBytes(out.toByteArray());) {
                        _dom = new SVGDOM(data);
                    }
                }
            } catch (Exception e) {
                if (_thread == this)
                    _error = e;
            } finally {
                if (_thread == this)
                    _thread = null;
            }
        }
    }

    @Override
    public void changeVariant(int delta) {
        super.changeVariant(delta);
        _thread = null;
        _dom = null;
        _error = null;
    }

    @Override
    public String variantTitle() {
        String url = _variants[_variantIdx];
        return url.substring(url.lastIndexOf("/") + 1);
    }
}
