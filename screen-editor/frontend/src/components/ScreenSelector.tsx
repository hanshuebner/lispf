import { DefScreen } from '../types';

interface ScreenSelectorProps {
  screens: DefScreen[];
  currentIndex: number;
  onSelect: (index: number) => void;
}

const containerStyle: React.CSSProperties = {
  display: 'flex',
  gap: '2px',
  padding: '4px 12px',
  backgroundColor: '#16213e',
  borderBottom: '1px solid #333',
  flexShrink: 0,
};

const tabStyle: React.CSSProperties = {
  padding: '4px 12px',
  fontSize: '12px',
  border: '1px solid #444',
  borderBottom: 'none',
  borderRadius: '4px 4px 0 0',
  cursor: 'pointer',
  backgroundColor: '#0f3460',
  color: '#888',
};

const activeTabStyle: React.CSSProperties = {
  ...tabStyle,
  backgroundColor: '#1a1a2e',
  color: '#e0e0e0',
  borderColor: '#555',
};

export default function ScreenSelector({ screens, currentIndex, onSelect }: ScreenSelectorProps) {
  return (
    <div style={containerStyle}>
      {screens.map((screen, i) => (
        <button
          key={i}
          style={i === currentIndex ? activeTabStyle : tabStyle}
          onClick={() => onSelect(i)}
        >
          {screen.name}
        </button>
      ))}
    </div>
  );
}
