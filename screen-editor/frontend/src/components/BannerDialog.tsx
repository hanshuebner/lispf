import { useState, useRef, useEffect } from 'react';
import { FONTS, renderBanner } from '../bigLetters';

interface BannerDialogProps {
  onConfirm: (lines: string[]) => void;
  onCancel: () => void;
}

const STORAGE_KEY_FONT = 'banner-last-font';
const STORAGE_KEY_SPACING = 'banner-last-spacing';

function loadStored<T>(key: string, fallback: T): T {
  try { const v = localStorage.getItem(key); return v !== null ? JSON.parse(v) : fallback; }
  catch { return fallback; }
}

export default function BannerDialog({ onConfirm, onCancel }: BannerDialogProps) {
  const [text, setText] = useState('');
  const [font, setFont] = useState(() => loadStored(STORAGE_KEY_FONT, FONTS[0].name));
  const [spacing, setSpacing] = useState(() => loadStored(STORAGE_KEY_SPACING, 2));
  const inputRef = useRef<HTMLInputElement>(null);
  useEffect(() => { inputRef.current?.focus(); }, []);

  const previewText = text.trim() || 'LISPF';
  const preview = renderBanner(previewText, font, spacing);
  const maxWidth = Math.max(0, ...preview.map(l => l.length));
  const tooWide = maxWidth > 80;
  const tooTall = preview.length > 21;

  const handleSubmit = () => {
    if (!text.trim()) return;
    onConfirm(renderBanner(text.trim(), font, spacing));
  };

  return (
    <div style={{
      position: 'absolute', inset: 0, display: 'flex', alignItems: 'center', justifyContent: 'center',
      backgroundColor: 'rgba(0,0,0,0.6)', zIndex: 1000,
    }} onClick={onCancel}>
      <div style={{
        backgroundColor: '#16213e', border: '1px solid #444', borderRadius: '6px',
        padding: '20px', minWidth: '400px', maxWidth: '90vw',
      }} onClick={e => e.stopPropagation()}>
        <div style={{ marginBottom: '12px', fontWeight: 'bold', color: '#50fa7b' }}>Insert Banner Text</div>
        <div style={{ display: 'flex', gap: '8px', marginBottom: '12px' }}>
          <select
            style={{
              padding: '6px 8px', backgroundColor: '#0f3460',
              border: '1px solid #444', borderRadius: '3px', color: '#e0e0e0',
              fontFamily: 'monospace', fontSize: '14px',
            }}
            value={font}
            onChange={e => { setFont(e.target.value); localStorage.setItem(STORAGE_KEY_FONT, JSON.stringify(e.target.value)); }}
          >
            {FONTS.map(f => (
              <option key={f.name} value={f.name}>{f.label}</option>
            ))}
          </select>
          <label style={{ display: 'flex', alignItems: 'center', gap: '4px', color: '#aaa', fontSize: '13px', whiteSpace: 'nowrap' }}>
            Gap {spacing}
            <input
              type="range" min={1} max={3} step={1}
              value={spacing}
              onChange={e => { const v = Number(e.target.value); setSpacing(v); localStorage.setItem(STORAGE_KEY_SPACING, JSON.stringify(v)); }}
              style={{ width: '60px', accentColor: '#50fa7b' }}
            />
          </label>
          <input
            ref={inputRef}
            style={{
              flex: 1, padding: '6px 8px', backgroundColor: '#0f3460',
              border: '1px solid #444', borderRadius: '3px', color: '#e0e0e0',
              fontFamily: 'monospace', fontSize: '14px',
            }}
            placeholder="Enter text..."
            value={text}
            onChange={e => setText(e.target.value)}
            onKeyDown={e => {
              if (e.key === 'Enter' && !tooWide && !tooTall) handleSubmit();
              if (e.key === 'Escape') onCancel();
            }}
          />
        </div>
        <div style={{
            backgroundColor: '#000', padding: '8px', borderRadius: '3px',
            marginBottom: '12px', overflowX: 'auto',
          }}>
            <pre style={{
              fontFamily: "'3270', 'IBM Plex Mono', 'Courier New', monospace",
              fontSize: '12px', lineHeight: '1.2', color: '#00ff41',
              margin: 0, whiteSpace: 'pre',
            }}>
              {preview.join('\n')}
            </pre>
            {tooWide && (
              <div style={{ color: '#ff5555', fontSize: '12px', marginTop: '4px' }}>
                Too wide ({maxWidth} cols) — maximum is 80. Use shorter text.
              </div>
            )}
            {tooTall && (
              <div style={{ color: '#ff5555', fontSize: '12px', marginTop: '4px' }}>
                Too tall ({preview.length} rows) — maximum is 21. Use a smaller font.
              </div>
            )}
          </div>
        <div style={{ display: 'flex', gap: '8px', justifyContent: 'flex-end' }}>
          <button style={{
            padding: '4px 12px', backgroundColor: '#333', border: '1px solid #555',
            borderRadius: '3px', color: '#ccc', cursor: 'pointer',
          }} onClick={onCancel}>Cancel</button>
          <button style={{
            padding: '4px 12px', backgroundColor: '#50fa7b', border: 'none',
            borderRadius: '3px', color: '#000', cursor: 'pointer', fontWeight: 'bold',
            opacity: text.trim() && !tooWide && !tooTall ? 1 : 0.5,
          }} onClick={handleSubmit} disabled={!text.trim() || tooWide}>OK</button>
        </div>
      </div>
    </div>
  );
}
