// Large character art generator using bitmap fonts from fonts/*.json.
// Each font file maps characters to arrays of strings where the character
// itself is used as the fill (IBM login screen style).

type FontData = Record<string, string[]>;

interface FontEntry {
  name: string;
  label: string;
  data: FontData;
}

const fontModules = import.meta.glob<FontData>('/fonts/*.json', { eager: true, import: 'default' });

function labelFromFilename(filename: string): string {
  const base = filename.replace(/^\/fonts\//, '').replace(/\.json$/, '');
  const [family, size] = base.split(/-(?=\d+$)/);
  const prettyFamily = family.replace(/-/g, ' ').replace(/\b\w/g, c => c.toUpperCase());
  return `${prettyFamily} (${size}px)`;
}

const fontEntries: FontEntry[] = Object.entries(fontModules)
  .map(([path, data]) => ({
    name: path.replace(/^\/fonts\//, '').replace(/\.json$/, ''),
    label: labelFromFilename(path),
    data: data as FontData,
  }))
  .sort((a, b) => a.label.localeCompare(b.label));

export interface FontInfo {
  name: string;
  label: string;
}

export const FONTS: FontInfo[] = fontEntries.map(({ name, label }) => ({ name, label }));

function getFont(fontName: string): FontData {
  const entry = fontEntries.find(f => f.name === fontName);
  return entry ? entry.data : fontEntries[0].data;
}

export function renderBanner(text: string, fontName: string, spacing: number = 1): string[] {
  const font = getFont(fontName);
  const space = font[' '];
  if (!space) return [text];
  const height = space.length;
  const lines = Array.from({ length: height }, () => '');
  const gap = ' '.repeat(spacing);

  for (const ch of text) {
    // Try the character as-is first, then uppercase, then fall back to space
    const raw = font[ch] || font[ch.toUpperCase()] || font[' '];
    // Trim each glyph left and right before assembling
    const minLeft = Math.min(...raw.map(r => r.length - r.trimStart().length));
    const maxRight = Math.max(...raw.map(r => r.trimEnd().length));
    const glyph = raw.map(r => r.substring(minLeft, Math.max(minLeft, maxRight)));
    for (let row = 0; row < height; row++) {
      if (lines[row].length > 0) lines[row] += gap;
      lines[row] += (glyph[row] || '');
    }
  }

  // Trim blank rows from top and bottom
  while (lines.length > 0 && lines[0].trim() === '') lines.shift();
  while (lines.length > 0 && lines[lines.length - 1].trim() === '') lines.pop();

  // Trim common leading spaces and trailing spaces from all rows
  if (lines.length > 0) {
    const minLeading = Math.min(...lines.map(l => l.length - l.trimStart().length));
    if (minLeading > 0) {
      for (let i = 0; i < lines.length; i++) lines[i] = lines[i].substring(minLeading);
    }
    for (let i = 0; i < lines.length; i++) lines[i] = lines[i].trimEnd();
  }

  return lines;
}
