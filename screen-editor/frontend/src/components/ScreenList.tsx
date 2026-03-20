interface ScreenListProps {
  screens: string[];
  menus: string[];
  currentScreen: string | null;
  currentMenu: string | null;
  onSelect: (name: string) => void;
  onSelectMenu: (name: string) => void;
  onNew: () => void;
  onNewMenu: () => void;
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
  margin: '4px 8px 8px',
  padding: '5px 0',
  backgroundColor: '#0f3460',
  border: '1px solid #444',
  borderRadius: '3px',
  color: '#e0e0e0',
  cursor: 'pointer',
  fontSize: '14px',
  textAlign: 'center',
};

export default function ScreenList({
  screens, menus, currentScreen, currentMenu,
  onSelect, onSelectMenu, onNew, onNewMenu,
}: ScreenListProps) {
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
      <div style={headerStyle}>Menus</div>
      <div style={listStyle}>
        {menus.map(name => (
          <div
            key={name}
            style={name === currentMenu ? activeItemStyle : itemStyle}
            onClick={() => onSelectMenu(name)}
          >
            {name}
          </div>
        ))}
      </div>
      <button style={newButtonStyle} onClick={onNewMenu}>New Menu</button>
    </div>
  );
}
