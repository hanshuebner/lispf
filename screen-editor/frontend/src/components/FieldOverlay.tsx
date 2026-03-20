import { Field } from '../types';

interface FieldOverlayProps {
  field: Field;
  index: number;
  cellWidth: number;
  cellHeight: number;
  selected: boolean;
  onClick: (index: number) => void;
}

const FIELD_COLORS = [
  'rgba(80, 250, 123, 0.2)',   // green
  'rgba(139, 233, 253, 0.2)',  // cyan
  'rgba(255, 121, 198, 0.2)',  // pink
  'rgba(241, 250, 140, 0.2)',  // yellow
  'rgba(189, 147, 249, 0.2)',  // purple
  'rgba(255, 85, 85, 0.2)',    // red
  'rgba(92, 92, 255, 0.2)',    // blue
  'rgba(255, 184, 108, 0.2)',  // orange
];

const TERMINAL_COLORS: Record<string, string> = {
  default: '#e0e0e0',
  blue: '#5c5cff',
  red: '#ff5555',
  pink: '#ff79c6',
  green: '#50fa7b',
  turquoise: '#8be9fd',
  yellow: '#f1fa8c',
  white: '#f8f8f2',
};

export default function FieldOverlay({ field, index, cellWidth, cellHeight, selected, onClick }: FieldOverlayProps) {
  const bgColor = FIELD_COLORS[index % FIELD_COLORS.length];
  const borderColor = selected
    ? TERMINAL_COLORS[field.color] || '#50fa7b'
    : 'transparent';

  return (
    <>
      {/* Attribute byte marker */}
      <div
        onClick={(e) => { e.stopPropagation(); onClick(index); }}
        title={`Attribute byte at (${field.fromRow},${field.fromCol})`}
        style={{
          position: 'absolute',
          left: field.fromCol * cellWidth,
          top: field.fromRow * cellHeight,
          width: cellWidth,
          height: cellHeight,
          backgroundColor: 'rgba(255, 255, 255, 0.08)',
          borderRight: '1px dotted rgba(255, 255, 255, 0.3)',
          boxSizing: 'border-box',
          cursor: 'pointer',
          pointerEvents: 'auto',
          zIndex: selected ? 10 : 1,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          fontSize: '7px',
          color: 'rgba(255, 255, 255, 0.3)',
        }}
      >
        A
      </div>
      {/* Field content area */}
      <div
        onClick={(e) => { e.stopPropagation(); onClick(index); }}
        title={`${field.name || `field ${index}`} (${field.fromRow},${field.fromCol}) len=${field.len}${(field.repeat || 1) > 1 ? ` repeat=${field.repeat}` : ''}`}
        style={{
          position: 'absolute',
          left: (field.fromCol + 1) * cellWidth,
          top: field.fromRow * cellHeight,
          width: field.len * cellWidth,
          height: (field.repeat || 1) * cellHeight,
          backgroundColor: bgColor,
          border: `1px ${field.write ? 'dashed' : 'solid'} ${borderColor}`,
          boxSizing: 'border-box',
          cursor: 'pointer',
          pointerEvents: 'auto',
          zIndex: selected ? 10 : 1,
        }}
      />
    </>
  );
}
