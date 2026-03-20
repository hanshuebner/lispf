import { useState, useRef } from 'react';
import { KeyAction, KeyActionType, AidKey, AID_KEYS } from '../types';

interface KeyActionsPanelProps {
  keys: KeyAction[];
  onChange: (keys: KeyAction[]) => void;
}

const panelStyle: React.CSSProperties = {
  padding: '12px',
  backgroundColor: '#16213e',
  borderTop: '1px solid #333',
  fontSize: '14px',
};

const rowStyle: React.CSSProperties = {
  display: 'flex',
  alignItems: 'center',
  gap: '4px',
  marginBottom: '4px',
};

const baseInput: React.CSSProperties = {
  padding: '2px 3px',
  backgroundColor: '#0f3460',
  border: '1px solid #444',
  borderRadius: '2px',
  color: '#e0e0e0',
  fontFamily: 'monospace',
  fontSize: '11px',
  minWidth: 0,
};

const dragHandleStyle: React.CSSProperties = {
  cursor: 'grab',
  color: '#666',
  fontSize: '11px',
  flexShrink: 0,
  userSelect: 'none',
  lineHeight: 1,
  padding: '0 2px',
};

function aidKeyDisplayName(key: AidKey): string {
  if (key === 'enter' || key === 'clear') return key.charAt(0).toUpperCase() + key.slice(1);
  return key.toUpperCase();
}

export default function KeyActionsPanel({ keys, onChange }: KeyActionsPanelProps) {
  const [collapsed, setCollapsed] = useState(false);
  const [dragIndex, setDragIndex] = useState<number | null>(null);
  const [dropIndex, setDropIndex] = useState<number | null>(null);
  const dragNode = useRef<HTMLDivElement | null>(null);

  const usedKeys = new Set(keys.map(k => k.aidKey));
  const availableKeys = AID_KEYS.filter(k => !usedKeys.has(k));

  const updateKey = (index: number, patch: Partial<KeyAction>) => {
    const updated = [...keys];
    updated[index] = { ...updated[index], ...patch };
    if (patch.action && patch.action !== 'goto') {
      delete updated[index].gotoScreen;
    }
    onChange(updated);
  };

  const removeKey = (index: number) => {
    onChange(keys.filter((_, i) => i !== index));
  };

  const addKey = () => {
    const aidKey = availableKeys[0] || 'enter';
    onChange([...keys, { aidKey, label: aidKeyDisplayName(aidKey), action: 'handler' }]);
  };

  const handleDragStart = (e: React.DragEvent, index: number) => {
    setDragIndex(index);
    dragNode.current = e.currentTarget as HTMLDivElement;
    e.dataTransfer.effectAllowed = 'move';
    // Make the drag image slightly transparent
    requestAnimationFrame(() => {
      if (dragNode.current) dragNode.current.style.opacity = '0.4';
    });
  };

  const handleDragEnd = () => {
    if (dragNode.current) dragNode.current.style.opacity = '1';
    if (dragIndex !== null && dropIndex !== null && dragIndex !== dropIndex) {
      const reordered = [...keys];
      const [moved] = reordered.splice(dragIndex, 1);
      reordered.splice(dropIndex, 0, moved);
      onChange(reordered);
    }
    setDragIndex(null);
    setDropIndex(null);
    dragNode.current = null;
  };

  const handleDragOver = (e: React.DragEvent, index: number) => {
    e.preventDefault();
    e.dataTransfer.dropEffect = 'move';
    setDropIndex(index);
  };

  return (
    <div style={panelStyle}>
      <div
        style={{ marginBottom: '8px', fontWeight: 'bold', color: '#50fa7b', cursor: 'pointer', userSelect: 'none' }}
        onClick={() => setCollapsed(c => !c)}
      >
        {collapsed ? '\u25b8' : '\u25be'} Key Actions ({keys.length})
      </div>
      {!collapsed && (
        <>
          {keys.length === 0 && (
            <div style={{ color: '#666', fontSize: '12px', marginBottom: '8px' }}>No keys configured</div>
          )}
          {keys.map((k, i) => (
            <div
              key={i}
              draggable
              onDragStart={e => handleDragStart(e, i)}
              onDragEnd={handleDragEnd}
              onDragOver={e => handleDragOver(e, i)}
              style={{
                ...rowStyle,
                borderTop: dropIndex === i && dragIndex !== null && dragIndex > i
                  ? '2px solid #50fa7b' : '2px solid transparent',
                borderBottom: dropIndex === i && dragIndex !== null && dragIndex < i
                  ? '2px solid #50fa7b' : '2px solid transparent',
              }}
            >
              <span style={dragHandleStyle} title="Drag to reorder">{'\u2630'}</span>
              <select
                style={{ ...baseInput, width: '68px', flexShrink: 0 }}
                value={k.aidKey}
                onChange={e => {
                  const newKey = e.target.value as AidKey;
                  updateKey(i, { aidKey: newKey, label: k.label || aidKeyDisplayName(newKey) });
                }}
              >
                <option value={k.aidKey}>{k.aidKey.toUpperCase()}</option>
                {availableKeys.map(ak => (
                  <option key={ak} value={ak}>{ak.toUpperCase()}</option>
                ))}
              </select>
              <input
                style={{ ...baseInput, flex: '1 1 0' }}
                type="text"
                placeholder="Label"
                value={k.label}
                onChange={e => updateKey(i, { label: e.target.value })}
              />
              <select
                style={{ ...baseInput, width: '62px', flexShrink: 0 }}
                value={k.action}
                onChange={e => updateKey(i, { action: e.target.value as KeyActionType })}
              >
                <option value="handler">Handler</option>
                <option value="back">Back</option>
                <option value="goto">Go to</option>
              </select>
              {k.action === 'goto' && (
                <input
                  style={{ ...baseInput, width: '56px', flexShrink: 0 }}
                  type="text"
                  placeholder="screen"
                  value={k.gotoScreen || ''}
                  onChange={e => updateKey(i, { gotoScreen: e.target.value })}
                />
              )}
              <label style={{ display: 'flex', alignItems: 'center', gap: '2px', fontSize: '11px', color: '#aaa', flexShrink: 0 }}>
                <input
                  type="checkbox"
                  checked={!!k.hidden}
                  onChange={e => updateKey(i, { hidden: e.target.checked || undefined })}
                  style={{ margin: 0 }}
                />
                Hidden
              </label>
              <button
                style={{
                  padding: '1px 5px', backgroundColor: '#ff5555', border: 'none',
                  borderRadius: '2px', color: '#fff', cursor: 'pointer', fontSize: '10px', flexShrink: 0,
                }}
                onClick={() => removeKey(i)}
              >{'\u2715'}</button>
            </div>
          ))}
          {availableKeys.length > 0 && (
            <button
              style={{
                padding: '4px 12px', backgroundColor: '#50fa7b', border: 'none',
                borderRadius: '3px', color: '#000', cursor: 'pointer', fontSize: '13px',
                fontWeight: 'bold', marginTop: '8px',
              }}
              onClick={addKey}
            >Add Key</button>
          )}
        </>
      )}
    </div>
  );
}
