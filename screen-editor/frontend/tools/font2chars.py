#!/usr/bin/env python3
"""Render TrueType fonts as character art.

Each glyph is rasterized to a grid where "on" pixels are replaced by the
character itself.  Outputs a JSON object mapping each printable ASCII
character (0x20-0x7E) to an array of strings (one per row).

Usage:
    python3 font2chars.py FONTPATH [--size SIZE] [--threshold THRESHOLD] [-o OUTPUT]

Examples:
    python3 font2chars.py /path/to/IBMPlexMono-Regular.ttf
    python3 font2chars.py /path/to/Data70.ttf --size 16 -o data70.json
"""

import argparse
import json
import sys
from PIL import Image, ImageDraw, ImageFont


PRINTABLE = [chr(i) for i in range(0x20, 0x7F)]


def rasterize_font(font_path, size=12, threshold=128):
    font = ImageFont.truetype(font_path, size)

    # Determine a uniform cell size from the widest/tallest glyph.
    max_w, max_h = 0, 0
    for ch in PRINTABLE:
        bbox = font.getbbox(ch)
        w = bbox[2] - bbox[0]
        h = bbox[3] - bbox[1]
        if w > max_w:
            max_w = w
        if h > max_h:
            max_h = h

    # Use the font metrics for consistent vertical placement.
    ascent, descent = font.getmetrics()
    cell_h = ascent + descent
    cell_w = max_w + 2  # small horizontal padding

    result = {}

    for ch in PRINTABLE:
        img = Image.new("L", (cell_w, cell_h), 0)
        draw = ImageDraw.Draw(img)

        # Center horizontally within the cell; use consistent y=0 baseline.
        bbox = font.getbbox(ch)
        glyph_w = bbox[2] - bbox[0]
        x_offset = (cell_w - glyph_w) // 2 - bbox[0]
        y_offset = 0

        draw.text((x_offset, y_offset), ch, fill=255, font=font)

        rows = []
        for y in range(cell_h):
            row = ""
            for x in range(cell_w):
                row += ch if img.getpixel((x, y)) > threshold else " "
            rows.append(row)

        result[ch] = rows

    # Trim rows that are blank across ALL glyphs (global trim for alignment).
    blank = " " * cell_w
    top_trim = 0
    while top_trim < cell_h and all(
        result[ch][top_trim] == blank for ch in PRINTABLE
    ):
        top_trim += 1
    bot_trim = cell_h
    while bot_trim > top_trim and all(
        result[ch][bot_trim - 1] == blank for ch in PRINTABLE
    ):
        bot_trim -= 1

    # Ensure at least one row survives (for the space character).
    if top_trim >= bot_trim:
        bot_trim = top_trim + 1

    for ch in PRINTABLE:
        result[ch] = result[ch][top_trim:bot_trim]

    trimmed_h = bot_trim - top_trim
    return result, cell_w, trimmed_h


def preview(result, chars="AaBb@#0"):
    for ch in chars:
        if ch in result:
            print(f"--- '{ch}' ---")
            for line in result[ch]:
                print(line)
            print()


def main():
    parser = argparse.ArgumentParser(description="Render TTF fonts as character art")
    parser.add_argument("font", help="Path to a TrueType font file")
    parser.add_argument("--size", type=int, default=12, help="Font size in points (default: 12)")
    parser.add_argument("--threshold", type=int, default=128, help="Pixel threshold 0-255 (default: 128)")
    parser.add_argument("-o", "--output", help="Output JSON file (default: stdout)")
    parser.add_argument("--preview", action="store_true", help="Print a preview of a few characters")
    args = parser.parse_args()

    result, cell_w, cell_h = rasterize_font(args.font, args.size, args.threshold)

    if args.preview:
        print(f"Cell size: {cell_w}x{cell_h}")
        preview(result)
        return

    output = json.dumps(result, indent=2, ensure_ascii=False)

    if args.output:
        with open(args.output, "w") as f:
            f.write(output)
            f.write("\n")
        print(f"Wrote {len(result)} characters ({cell_w}x{cell_h} grid) to {args.output}", file=sys.stderr)
    else:
        print(output)


if __name__ == "__main__":
    main()
