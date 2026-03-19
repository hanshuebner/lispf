interface TitleBarProps {
  screenName: string | null;
  screenPath: string | null;
}

const titleBarStyle: React.CSSProperties = {
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'space-between',
  padding: '10px 16px',
  backgroundColor: '#0d1b2a',
  borderBottom: '1px solid #333',
  flexShrink: 0,
};

const appTitleStyle: React.CSSProperties = {
  fontSize: '22px',
  fontWeight: 'bold',
  color: '#50fa7b',
};

const screenInfoStyle: React.CSSProperties = {
  fontSize: '14px',
  color: '#999',
  fontFamily: 'monospace',
};

export default function TitleBar({ screenName, screenPath }: TitleBarProps) {
  return (
    <div style={titleBarStyle}>
      <span style={appTitleStyle}>3270 Screen Editor</span>
      {screenName && (
        <span style={screenInfoStyle}>
          {screenName}{screenPath ? ` \u2014 ${screenPath}` : ''}
        </span>
      )}
    </div>
  );
}
