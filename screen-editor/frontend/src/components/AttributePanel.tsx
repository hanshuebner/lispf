import { Field, Selection, Color, Highlighting } from '../types';

interface AttributePanelProps {
  field: Field | null;
  selection: Selection | null;
  canCreateField: boolean;
  validationError: string | null;
  maxRepeat: number;
  onChange: (field: Field) => void;
  onDelete: () => void;
  onCreateField: () => void;
}

const panelStyle: React.CSSProperties = {
  padding: '12px',
  backgroundColor: '#16213e',
  fontSize: '14px',
};

const rowStyle: React.CSSProperties = {
  display: 'flex',
  justifyContent: 'space-between',
  alignItems: 'center',
  marginBottom: '6px',
};

const labelStyle: React.CSSProperties = {
  color: '#b0b0b0',
};

const inputStyle: React.CSSProperties = {
  width: '70px',
  padding: '2px 4px',
  backgroundColor: '#0f3460',
  border: '1px solid #444',
  borderRadius: '2px',
  color: '#e0e0e0',
  textAlign: 'right',
  fontFamily: 'monospace',
  fontSize: '12px',
};

const selectStyle: React.CSSProperties = {
  ...inputStyle,
  width: '120px',
  textAlign: 'left',
};

const textInputStyle: React.CSSProperties = {
  ...inputStyle,
  width: '120px',
  textAlign: 'left',
};

const deleteButtonStyle: React.CSSProperties = {
  marginTop: '16px',
  padding: '6px 12px',
  backgroundColor: '#ff5555',
  border: 'none',
  borderRadius: '3px',
  color: '#fff',
  cursor: 'pointer',
  width: '100%',
  fontSize: '13px',
};

const createButtonStyle: React.CSSProperties = {
  padding: '8px 12px',
  backgroundColor: '#50fa7b',
  border: 'none',
  borderRadius: '3px',
  color: '#000',
  cursor: 'pointer',
  width: '100%',
  fontSize: '14px',
  fontWeight: 'bold',
};

const COLORS: Color[] = ['default', 'blue', 'red', 'pink', 'green', 'turquoise', 'yellow', 'white'];
const HIGHLIGHTS: Highlighting[] = ['default', 'blink', 'reverse-video', 'underscore'];

export default function AttributePanel({
  field, selection, canCreateField, validationError, maxRepeat, onChange, onDelete, onCreateField,
}: AttributePanelProps) {
  // Selection state — show "New Field" button
  if (!field && selection) {
    const isSingleRow = selection.fromRow === selection.toRow;
    const width = selection.toCol - selection.fromCol + 1;
    const height = selection.toRow - selection.fromRow + 1;
    return (
      <div style={panelStyle}>
        <div style={{ marginBottom: '12px', fontWeight: 'bold', color: '#50fa7b' }}>
          Selection
        </div>
        <div style={rowStyle}>
          <span style={labelStyle}>{isSingleRow ? 'Row' : 'Rows'}</span>
          <span>{isSingleRow ? selection.fromRow : `${selection.fromRow} - ${selection.toRow}`}</span>
        </div>
        <div style={rowStyle}>
          <span style={labelStyle}>Columns</span>
          <span>{selection.fromCol} - {selection.toCol}</span>
        </div>
        <div style={rowStyle}>
          <span style={labelStyle}>Size</span>
          <span>{width} x {height}</span>
        </div>
        {canCreateField ? (
          <button style={{ ...createButtonStyle, marginTop: '16px' }} onClick={onCreateField}>
            New Field
          </button>
        ) : !isSingleRow ? null : (
          <div style={{ color: '#ff5555', marginTop: '16px', textAlign: 'center' }}>
            Selection overlaps a field or contains text.
          </div>
        )}
      </div>
    );
  }

  // No selection, no field — empty panel
  if (!field) {
    return (
      <div style={panelStyle}>
        <div style={{ color: '#666', marginTop: '20px', textAlign: 'center', fontSize: '13px' }}>
          No field selected
        </div>
      </div>
    );
  }

  // Field selected — show attributes
  const update = (patch: Partial<Field>) => onChange({ ...field, ...patch });

  return (
    <div style={panelStyle}>
      <div style={{ marginBottom: '12px', fontWeight: 'bold', color: '#50fa7b' }}>
        Field Attributes
      </div>
      <div style={{ color: '#888', fontSize: '11px', marginBottom: '8px' }}>
        Attribute byte at column {field.fromCol}. Content visible from column {field.fromCol + 1}.
        {field.fromCol === 0 && (
          <span style={{ color: '#f1fa8c' }}> Content starts at column 1 on the terminal.</span>
        )}
      </div>

      <div style={rowStyle}>
        <span style={labelStyle}>Row</span>
        <input
          style={inputStyle}
          type="number"
          min={1} max={21}
          value={field.fromRow}
          onChange={e => {
            const v = parseInt(e.target.value) || 1;
            update({ fromRow: Math.max(1, Math.min(21, v)) });
          }}
        />
      </div>
      <div style={rowStyle}>
        <span style={labelStyle}>Col</span>
        <input
          style={inputStyle}
          type="number"
          min={0} max={79}
          value={field.fromCol}
          onChange={e => update({ fromCol: parseInt(e.target.value) || 0 })}
        />
      </div>
      <div style={rowStyle}>
        <span style={labelStyle}>Length</span>
        <input
          style={inputStyle}
          type="number"
          min={1} max={80}
          value={field.len}
          onChange={e => update({ len: parseInt(e.target.value) || 1 })}
        />
      </div>
      <div style={rowStyle}>
        <span style={labelStyle}>Repeat</span>
        <input
          style={inputStyle}
          type="number"
          min={1} max={maxRepeat}
          value={field.repeat || 1}
          onChange={e => {
            const v = parseInt(e.target.value) || 1;
            update({ repeat: Math.max(1, Math.min(maxRepeat, v)) });
          }}
        />
      </div>
      <div style={rowStyle}>
        <span style={labelStyle}>Name</span>
        <input
          style={{
            ...textInputStyle,
            ...(validationError ? { borderColor: '#ff5555' } : {}),
          }}
          type="text"
          value={field.name || ''}
          onChange={e => update({ name: e.target.value || null })}
          autoFocus={!!validationError}
        />
      </div>
      {validationError && (
        <div style={{ color: '#ff5555', fontSize: '11px', marginBottom: '6px' }}>
          {validationError}
        </div>
      )}

      <hr style={{ borderColor: '#333', margin: '10px 0' }} />

      <div style={rowStyle}>
        <span style={labelStyle}>Write</span>
        <input type="checkbox" checked={field.write} onChange={e => update({ write: e.target.checked })} />
      </div>
      <div style={rowStyle}>
        <span style={labelStyle}>Intense</span>
        <input type="checkbox" checked={field.intense} onChange={e => update({ intense: e.target.checked })} />
      </div>
      <div style={rowStyle}>
        <span style={labelStyle}>Hidden</span>
        <input type="checkbox" checked={field.hidden} onChange={e => update({ hidden: e.target.checked })} />
      </div>
      <div style={rowStyle}>
        <span style={labelStyle}>Numeric Only</span>
        <input type="checkbox" checked={field.numericOnly} onChange={e => update({ numericOnly: e.target.checked })} />
      </div>
      <div style={rowStyle}>
        <span style={labelStyle}>Autoskip</span>
        <input type="checkbox" checked={field.autoskip} onChange={e => update({ autoskip: e.target.checked })} />
      </div>
      <div style={rowStyle}>
        <span style={labelStyle}>Keepspaces</span>
        <input type="checkbox" checked={field.keepspaces} onChange={e => update({ keepspaces: e.target.checked })} />
      </div>
      <div style={rowStyle}>
        <span style={labelStyle}>Position Only</span>
        <input type="checkbox" checked={field.positionOnly} onChange={e => update({ positionOnly: e.target.checked })} />
      </div>
      <div style={rowStyle}>
        <span style={labelStyle}>Default</span>
        <input type="checkbox" checked={field.default} onChange={e => update({ default: e.target.checked })} />
      </div>
      <div style={rowStyle}>
        <span style={labelStyle}>Transient</span>
        <input type="checkbox" checked={field.transient} onChange={e => update({ transient: e.target.checked })} />
      </div>

      <hr style={{ borderColor: '#333', margin: '10px 0' }} />

      <div style={rowStyle}>
        <span style={labelStyle}>Color</span>
        <select
          style={selectStyle}
          value={field.color}
          onChange={e => update({ color: e.target.value as Color })}
        >
          {COLORS.map(c => <option key={c} value={c}>{c}</option>)}
        </select>
      </div>
      <div style={rowStyle}>
        <span style={labelStyle}>Highlighting</span>
        <select
          style={selectStyle}
          value={field.highlighting}
          onChange={e => update({ highlighting: e.target.value as Highlighting })}
        >
          {HIGHLIGHTS.map(h => <option key={h} value={h}>{h}</option>)}
        </select>
      </div>

      <button style={deleteButtonStyle} onClick={onDelete}>
        Delete Field
      </button>
    </div>
  );
}
