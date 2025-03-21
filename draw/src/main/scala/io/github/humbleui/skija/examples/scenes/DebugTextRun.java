package io.github.humbleui.skija.examples.scenes;

import io.github.humbleui.types.*;
import io.github.humbleui.skija.*;
import io.github.humbleui.skija.shaper.*;
import io.github.humbleui.types.*;

public class DebugTextRun {
    public RunInfo _info;
    public Font    _font;
    public Rect    _bounds;
    public short[] _glyphs;
    public Point[] _positions;
    public int[]   _clusters;

    public DebugTextRun(RunInfo _info, Font _font, Rect _bounds, short[] _glyphs, Point[] _positions, int[] _clusters) {
        this._info = _info;
        this._font = _font;
        this._bounds = _bounds;
        this._glyphs = _glyphs;
        this._positions = _positions;
        this._clusters = _clusters;
    }

    public RunInfo getInfo() {
        return _info;
    }

    public Font getFont() {
        return _font;
    }

    public Rect getBounds() {
        return _bounds;
    }

    public short[] getGlyphs() {
        return _glyphs;
    }

    public Point[] getPositions() {
        return _positions;
    }

    public int[] getClusters() {
        return _clusters;
    }
}
