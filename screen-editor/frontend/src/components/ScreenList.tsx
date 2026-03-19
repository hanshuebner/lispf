interface ScreenListProps {
  screens: string[];
  currentScreen: string | null;
  onSelect: (name: string) => void;
  onNew: () => void;
}

const sidebarStyle: React.CSSProperties = {
  width: '180px',
  backgroundColor: '#16213e',
  borderRight: '1px solid #333',
  overflowY: 'auto',
  flexShrink: 0,
  display: 'flex',
  flexDirection: 'column',
};

const headerStyle: React.CSSProperties = {
  padding: '10px 12px 6px',
  fontSize: '14px',
  color: '#999',
  textTransform: 'uppercase',
  letterSpacing: '0.5px',
};

const listStyle: React.CSSProperties = {
  flex: 1,
  overflowY: 'auto',
  padding: '0 4px',
};

const itemStyle: React.CSSProperties = {
  padding: '5px 8px',
  cursor: 'pointer',
  borderRadius: '3px',
  fontSize: '14px',
  fontFamily: 'monospace',
  color: '#ccc',
};

const activeItemStyle: React.CSSProperties = {
  ...itemStyle,
  backgroundColor: '#0f3460',
  color: '#50fa7b',
};

const newButtonStyle: React.CSSProperties = {
  margin: '8px',
  padding: '5px 0',
  backgroundColor: '#0f3460',
  border: '1px solid #444',
  borderRadius: '3px',
  color: '#e0e0e0',
  cursor: 'pointer',
  fontSize: '14px',
  textAlign: 'center',
};

export default function ScreenList({ screens, currentScreen, onSelect, onNew }: ScreenListProps) {
  return (
    <div style={sidebarStyle}>
      <div style={headerStyle}>Screens</div>
      <div style={listStyle}>
        {screens.map(name => (
          <div
            key={name}
            style={name === currentScreen ? activeItemStyle : itemStyle}
            onClick={() => onSelect(name)}
          >
            {name}
          </div>
        ))}
      </div>
      <button style={newButtonStyle} onClick={onNew}>New Screen</button>
    </div>
  );
}
