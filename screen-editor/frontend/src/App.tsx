import { useState, useCallback, useEffect, useMemo, useRef } from 'react';
import { DefScreen, Field, Selection, KeyAction, AidKey, makeDefaultField } from './types';
import { listScreens, loadScreen, saveScreen } from './api';
import TitleBar from './components/Toolbar';
import ScreenList from './components/ScreenList';
import ScreenGrid from './components/ScreenGrid';
import AttributePanel from './components/AttributePanel';
import KeyActionsPanel from './components/KeyActionsPanel';
import BannerDialog from './components/BannerDialog';

const EMPTY_ROWS = Array.from({ length: 24 }, () => '');

// --- Welcome/Help screen ---

function cols(left: string, right: string) {
  return left.padEnd(40) + right.padEnd(40);
}
const TITLE_ROWS = new Set([0, 7, 15]);
const WELCOME_ROWS = [
  cols('Getting Started', 'Fields'),
  cols('', ''),
  cols('Pick a screen on the left or', 'Drag blank cells on one row, then'),
  cols('click "New Screen" to create one.', 'click "New Field" in right panel.'),
  cols('Click grid to place cursor & type.', 'Every field needs a unique name.'),
  cols('Backspace deletes to the left.', 'Edit attributes in right panel.'),
  cols('', ''),
  cols('Moving Content', 'Shortcuts'),
  cols('', ''),
  cols('Drag to select (multi-row OK).', 'Cmd/Ctrl+S        Save'),
  cols('Arrow keys move the preview.', 'Cmd/Ctrl+Z        Undo'),
  cols('Enter confirms, Escape cancels.', 'Cmd/Ctrl+Shift+Z  Redo'),
  cols('Delete clears the selection.', 'Escape            Cancel'),
  cols('', 'F1                Help'),
  cols('', ''),
  cols('Framework Lines', ''),
  cols('', ''),
  cols('Lines 1, 23, 24 are reserved by the', ''),
  cols('framework and cannot be edited.', ''),
  cols('1: Screen name, app name, time', ''),
  cols('23: Error messages', ''),
  cols('24: Key labels (PF3, Enter, ...)', ''),
  '',
  '',
]
  .concat(Array(24).fill(''))
  .slice(0, 24)
  .map(r => r.padEnd(80).substring(0, 80));

function HelpScreen({ statusText }: { statusText: string }) {
  return (
    <div style={{ display: 'inline-block' }}>
      <pre className="screen-grid-pre">
        {WELCOME_ROWS.map((row, i) => (
          <span key={i} style={TITLE_ROWS.has(i) ? { color: '#fff' } : undefined}>
            {row}{i < 23 ? '\n' : ''}
          </span>
        ))}
      </pre>
      <div className="screen-grid-status"><span>{statusText}</span><span /></div>
    </div>
  );
}

// --- Confirm dialog ---

function ConfirmDialog({ message, onConfirm, onCancel }: {
  message: string;
  onConfirm: () => void;
  onCancel: () => void;
}) {
  return (
    <div style={{
      position: 'absolute', inset: 0, display: 'flex', alignItems: 'center', justifyContent: 'center',
      backgroundColor: 'rgba(0,0,0,0.6)', zIndex: 1000,
    }} onClick={onCancel}>
      <div style={{
        backgroundColor: '#16213e', border: '1px solid #444', borderRadius: '6px',
        padding: '20px', minWidth: '300px', maxWidth: '400px',
      }} onClick={e => e.stopPropagation()}>
        <div style={{ marginBottom: '16px', color: '#e0e0e0', fontSize: '14px' }}>{message}</div>
        <div style={{ display: 'flex', gap: '8px', justifyContent: 'flex-end' }}>
          <button style={{
            padding: '4px 12px', backgroundColor: '#333', border: '1px solid #555',
            borderRadius: '3px', color: '#ccc', cursor: 'pointer',
          }} onClick={onCancel}>Cancel</button>
          <button style={{
            padding: '4px 12px', backgroundColor: '#ff5555', border: 'none',
            borderRadius: '3px', color: '#fff', cursor: 'pointer', fontWeight: 'bold',
          }} onClick={onConfirm}>Discard</button>
        </div>
      </div>
    </div>
  );
}

// --- New screen dialog ---

function NewScreenDialog({ onConfirm, onCancel }: {
  onConfirm: (name: string) => void;
  onCancel: () => void;
}) {
  const [name, setName] = useState('');
  const inputRef = useRef<HTMLInputElement>(null);
  useEffect(() => { inputRef.current?.focus(); }, []);

  const handleSubmit = () => {
    const sanitized = name.trim().toLowerCase().replace(/[^a-z0-9-]/g, '-');
    if (sanitized) onConfirm(sanitized);
  };

  return (
    <div style={{
      position: 'absolute', inset: 0, display: 'flex', alignItems: 'center', justifyContent: 'center',
      backgroundColor: 'rgba(0,0,0,0.6)', zIndex: 1000,
    }} onClick={onCancel}>
      <div style={{
        backgroundColor: '#16213e', border: '1px solid #444', borderRadius: '6px',
        padding: '20px', minWidth: '300px',
      }} onClick={e => e.stopPropagation()}>
        <div style={{ marginBottom: '12px', fontWeight: 'bold', color: '#50fa7b' }}>New Screen</div>
        <input
          ref={inputRef}
          style={{
            width: '100%', padding: '6px 8px', backgroundColor: '#0f3460',
            border: '1px solid #444', borderRadius: '3px', color: '#e0e0e0',
            fontFamily: 'monospace', fontSize: '14px', boxSizing: 'border-box',
          }}
          placeholder="screen-name"
          value={name}
          onChange={e => setName(e.target.value)}
          onKeyDown={e => {
            if (e.key === 'Enter') handleSubmit();
            if (e.key === 'Escape') onCancel();
          }}
        />
        <div style={{ display: 'flex', gap: '8px', marginTop: '12px', justifyContent: 'flex-end' }}>
          <button style={{
            padding: '4px 12px', backgroundColor: '#333', border: '1px solid #555',
            borderRadius: '3px', color: '#ccc', cursor: 'pointer',
          }} onClick={onCancel}>Cancel</button>
          <button style={{
            padding: '4px 12px', backgroundColor: '#50fa7b', border: 'none',
            borderRadius: '3px', color: '#000', cursor: 'pointer', fontWeight: 'bold',
          }} onClick={handleSubmit}>Create</button>
        </div>
      </div>
    </div>
  );
}

// --- Helpers ---

function selectionCanCreateField(sel: Selection | null, fields: Field[], rows: string[]): boolean {
  if (!sel) return false;
  const overlaps = fields.some(f => {
    const fRepeat = f.repeat || 1;
    const fEndRow = f.fromRow + fRepeat - 1;
    return fEndRow >= sel.fromRow && f.fromRow <= sel.toRow
      && f.fromCol < sel.toCol + 1
      && f.fromCol + f.len > sel.fromCol;
  });
  if (overlaps) return false;
  for (let r = sel.fromRow; r <= sel.toRow; r++) {
    const rowStr = (rows[r] || '').padEnd(80, ' ');
    const content = rowStr.substring(sel.fromCol, sel.toCol + 1);
    if (content.trim().length !== 0) return false;
  }
  return true;
}

function maxRepeatForField(field: Field, fieldIndex: number, fields: Field[], rows: string[]): number {
  const MAX_APP_ROW = 21;
  let maxRepeat = MAX_APP_ROW - field.fromRow + 1;

  for (let r = field.fromRow + 1; r <= MAX_APP_ROW; r++) {
    const hasFieldConflict = fields.some((f, i) => {
      if (i === fieldIndex) return false;
      const fEndRow = f.fromRow + (f.repeat || 1) - 1;
      if (r < f.fromRow || r > fEndRow) return false;
      return f.fromCol < field.fromCol + field.len && f.fromCol + f.len > field.fromCol;
    });

    const rowStr = (rows[r] || '').padEnd(80, ' ');
    const content = rowStr.substring(field.fromCol, field.fromCol + field.len);
    const hasTextConflict = content.trim().length > 0;

    if (hasFieldConflict || hasTextConflict) {
      maxRepeat = r - field.fromRow;
      break;
    }
  }

  return Math.max(1, maxRepeat);
}

function aidKeyDisplayName(key: AidKey): string {
  if (key === 'enter' || key === 'clear') return key.charAt(0).toUpperCase() + key.slice(1);
  return key.toUpperCase();
}

function formatKeyLabelsRow(keys: KeyAction[]): string {
  if (keys.length === 0) return ''.padEnd(80);
  const labels = keys.map(k => `${aidKeyDisplayName(k.aidKey)} ${k.label}`);
  return (' ' + labels.join('  ')).padEnd(80).substring(0, 80);
}

function timestamp() {
  const d = new Date();
  return d.toLocaleTimeString('en-GB', { hour: '2-digit', minute: '2-digit', second: '2-digit' });
}

function screensEqual(a: DefScreen, b: DefScreen): boolean {
  if (a.name !== b.name) return false;
  if (a.rows.length !== b.rows.length) return false;
  for (let i = 0; i < a.rows.length; i++) if (a.rows[i] !== b.rows[i]) return false;
  if (a.fields.length !== b.fields.length) return false;
  if (JSON.stringify(a.fields) !== JSON.stringify(b.fields)) return false;
  return JSON.stringify(a.keys || []) === JSON.stringify(b.keys || []);
}

// --- Main App ---

export default function App() {
  const [screens, setScreens] = useState<string[]>([]);
  const [currentScreen, setCurrentScreen] = useState<string | null>(null);
  const [screen, setScreen] = useState<DefScreen | null>(null);
  const [selectedFieldIndex, setSelectedFieldIndex] = useState<number | null>(null);
  const [selection, setSelection] = useState<Selection | null>(null);
  const [dirty, setDirty] = useState(false);
  const [log, setLog] = useState<string[]>([]);
  const [validationError, setValidationError] = useState<string | null>(null);
  const [showNewDialog, setShowNewDialog] = useState(false);
  const [showHelp, setShowHelp] = useState(false);
  const [showBannerDialog, setShowBannerDialog] = useState(false);
  const [bannerOverlay, setBannerOverlay] = useState<string[] | null>(null);
  const [pendingSwitch, setPendingSwitch] = useState<string | null>(null);
  const undoStack = useRef<DefScreen[]>([]);
  const redoStack = useRef<DefScreen[]>([]);
  const cleanState = useRef<DefScreen | null>(null);
  const logEndRef = useRef<HTMLDivElement>(null);
  const appRef = useRef<HTMLDivElement>(null);
  const MAX_UNDO = 100;

  const addLog = useCallback((msg: string) => {
    setLog(prev => [...prev, `${timestamp()}  ${msg}`]);
  }, []);

  useEffect(() => {
    logEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [log]);

  // --- URL hash sync ---
  useEffect(() => {
    const hash = window.location.hash.replace('#', '');
    if (hash) setCurrentScreen(hash);
  }, []);

  useEffect(() => {
    if (currentScreen) {
      window.location.hash = currentScreen;
    } else {
      window.history.replaceState(null, '', window.location.pathname);
    }
  }, [currentScreen]);

  useEffect(() => {
    const handler = () => {
      const hash = window.location.hash.replace('#', '');
      if (hash && hash !== currentScreen) {
        handleSelectScreen(hash);
      }
    };
    window.addEventListener('hashchange', handler);
    return () => window.removeEventListener('hashchange', handler);
  });

  // --- Validation ---

  const validateCurrentField = useCallback((): boolean => {
    if (selectedFieldIndex === null || !screen) return true;
    const field = screen.fields[selectedFieldIndex];
    if (!field) return true;
    if (!field.name || field.name.trim() === '') {
      setValidationError('Field must have a name.');
      return false;
    }
    const nameLC = field.name.trim().toLowerCase();
    const duplicate = screen.fields.some((f, i) =>
      i !== selectedFieldIndex && f.name && f.name.trim().toLowerCase() === nameLC
    );
    if (duplicate) {
      setValidationError('Field name must be unique.');
      return false;
    }
    setValidationError(null);
    return true;
  }, [selectedFieldIndex, screen]);

  const validateAllFields = useCallback((): boolean => {
    if (!screen) return true;
    for (let i = 0; i < screen.fields.length; i++) {
      const f = screen.fields[i];
      if (!f.name || f.name.trim() === '') {
        setSelectedFieldIndex(i);
        setValidationError('Field must have a name.');
        addLog(`Validation error: field ${i} has no name`);
        return false;
      }
      const nameLC = f.name.trim().toLowerCase();
      const dup = screen.fields.some((other, j) =>
        j !== i && other.name && other.name.trim().toLowerCase() === nameLC
      );
      if (dup) {
        setSelectedFieldIndex(i);
        setValidationError('Field name must be unique.');
        addLog(`Validation error: duplicate field name "${f.name}"`);
        return false;
      }
    }
    setValidationError(null);
    return true;
  }, [screen, addLog]);

  const handleSelectField = useCallback((index: number | null) => {
    if (index !== selectedFieldIndex && !validateCurrentField()) return;
    setValidationError(null);
    setSelectedFieldIndex(index);
  }, [selectedFieldIndex, validateCurrentField]);

  const handleSelectionChange = useCallback((sel: Selection | null) => {
    if (sel && !validateCurrentField()) return;
    setValidationError(null);
    setSelection(sel);
  }, [validateCurrentField]);

  const canCreateField = useMemo(
    () => selectionCanCreateField(selection, screen?.fields ?? [], screen?.rows ?? []),
    [selection, screen?.fields, screen?.rows]
  );

  const fieldMaxRepeat = useMemo(() => {
    if (!screen || selectedFieldIndex === null || selectedFieldIndex >= screen.fields.length) return 21;
    return maxRepeatForField(screen.fields[selectedFieldIndex], selectedFieldIndex, screen.fields, screen.rows);
  }, [screen, selectedFieldIndex]);

  // --- Screen list ---

  const refreshScreenList = useCallback(async () => {
    try {
      const names = await listScreens();
      setScreens(names);
    } catch (e) {
      addLog(`Error listing screens: ${e instanceof Error ? e.message : String(e)}`);
    }
  }, [addLog]);

  useEffect(() => { refreshScreenList(); }, [refreshScreenList]);

  // --- Load screen (internal, no dirty check) ---

  const doLoadScreen = useCallback(async (name: string) => {
    try {
      addLog(`Loading ${name}...`);
      const data = await loadScreen(name);
      setCurrentScreen(name);
      setScreen(data);
      cleanState.current = data;
      setSelectedFieldIndex(null);
      setSelection(null);
      setDirty(false);
      setShowHelp(false);
      addLog(`Loaded ${name} (${data.fields.length} fields)`);
      undoStack.current = [];
      redoStack.current = [];
    } catch (e) {
      addLog(`Error loading ${name}: ${e instanceof Error ? e.message : String(e)}`);
    }
  }, [addLog]);

  // --- Select screen (with dirty check) ---

  const handleSelectScreen = useCallback((name: string) => {
    if (name === currentScreen) return;
    if (dirty) {
      setPendingSwitch(name);
    } else {
      doLoadScreen(name);
    }
  }, [currentScreen, dirty, doLoadScreen]);

  const handleConfirmSwitch = useCallback(() => {
    if (pendingSwitch) {
      doLoadScreen(pendingSwitch);
      setPendingSwitch(null);
    }
  }, [pendingSwitch, doLoadScreen]);

  // --- Auto-load from URL hash on startup ---
  useEffect(() => {
    if (currentScreen && !screen && screens.includes(currentScreen)) {
      doLoadScreen(currentScreen);
    }
  }, [currentScreen, screen, screens, doLoadScreen]);

  // --- New screen ---

  const handleNewConfirm = useCallback((sanitized: string) => {
    setShowNewDialog(false);
    const newScreen = { name: sanitized, rows: [...EMPTY_ROWS], fields: [] };
    setCurrentScreen(sanitized);
    setScreen(newScreen);
    cleanState.current = null; // new screen has no clean state yet
    setSelectedFieldIndex(null);
    setSelection(null);
    setDirty(true);
    setShowHelp(false);
    addLog(`New screen: ${sanitized}`);
    undoStack.current = [];
    redoStack.current = [];
  }, [addLog]);

  // --- Save ---

  const handleSave = useCallback(async () => {
    if (!screen) return;
    if (!validateAllFields()) return;
    try {
      addLog(`Saving ${screen.name}...`);
      await saveScreen(screen);
      cleanState.current = screen;
      setDirty(false);
      addLog(`Saved ${screen.name}`);
      refreshScreenList();
    } catch (e) {
      addLog(`Error saving: ${e instanceof Error ? e.message : String(e)}`);
    }
  }, [screen, refreshScreenList, validateAllFields, addLog]);

  // --- Undo/Redo with dirty tracking ---

  const updateScreen = useCallback((updater: (screen: DefScreen) => DefScreen) => {
    setScreen(prev => {
      if (!prev) return prev;
      undoStack.current.push(prev);
      if (undoStack.current.length > MAX_UNDO) undoStack.current.shift();
      redoStack.current = [];
      const next = updater(prev);
      setDirty(!cleanState.current || !screensEqual(next, cleanState.current));
      return next;
    });
  }, []);

  const undo = useCallback(() => {
    const prev = undoStack.current.pop();
    if (!prev) return;
    setScreen(current => {
      if (current) redoStack.current.push(current);
      setDirty(!cleanState.current || !screensEqual(prev, cleanState.current));
      return prev;
    });
    setSelectedFieldIndex(null);
    setSelection(null);
    setValidationError(null);
  }, []);

  const redo = useCallback(() => {
    const next = redoStack.current.pop();
    if (!next) return;
    setScreen(current => {
      if (current) undoStack.current.push(current);
      setDirty(!cleanState.current || !screensEqual(next, cleanState.current));
      return next;
    });
    setSelectedFieldIndex(null);
    setSelection(null);
    setValidationError(null);
  }, []);

  // --- Screen mutations ---

  const handleRowsChange = useCallback((rows: string[]) => {
    updateScreen(s => ({ ...s, rows }));
  }, [updateScreen]);

  const handleFieldUpdate = useCallback((index: number, field: Field) => {
    updateScreen(s => {
      const fields = [...s.fields];
      const max = maxRepeatForField(field, index, s.fields, s.rows);
      fields[index] = { ...field, repeat: Math.min(field.repeat, max) };
      return { ...s, fields };
    });
  }, [updateScreen]);

  const handleFieldDelete = useCallback((index: number) => {
    updateScreen(s => ({
      ...s,
      fields: s.fields.filter((_, i) => i !== index),
    }));
    setSelectedFieldIndex(null);
  }, [updateScreen]);

  const handleCreateField = useCallback(() => {
    if (!screen || !selection || !canCreateField) return;
    const len = selection.toCol - selection.fromCol + 1;
    const repeat = selection.toRow - selection.fromRow + 1;
    const newField = { ...makeDefaultField(selection.fromRow, selection.fromCol, len), repeat };
    updateScreen(s => ({ ...s, fields: [...s.fields, newField] }));
    setSelectedFieldIndex(screen.fields.length);
    setSelection(null);
    addLog(`Created field at row ${selection.fromRow}, col ${selection.fromCol}, len ${len}${repeat > 1 ? `, repeat ${repeat}` : ''}`);
  }, [screen, selection, canCreateField, updateScreen, addLog]);

  const handleKeysChange = useCallback((keys: KeyAction[]) => {
    updateScreen(s => ({ ...s, keys }));
  }, [updateScreen]);

  const handleBannerConfirm = useCallback((lines: string[]) => {
    setShowBannerDialog(false);
    setBannerOverlay(lines);
    addLog('Banner text ready — use arrows to position, Enter to place');
  }, [addLog]);

  const handleOverlayClear = useCallback(() => {
    setBannerOverlay(null);
  }, []);

  // --- Keyboard shortcuts ---

  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if (showNewDialog || showBannerDialog || pendingSwitch) return;
      if (e.key === 'F1') {
        e.preventDefault();
        setShowHelp(h => !h);
        return;
      }
      if (showHelp) {
        if (e.key === 'Escape') { e.preventDefault(); setShowHelp(false); }
        return;
      }
      if ((e.metaKey || e.ctrlKey) && e.key === 's') {
        e.preventDefault();
        handleSave();
      }
      if ((e.metaKey || e.ctrlKey) && e.key === 'z' && !e.shiftKey) {
        e.preventDefault();
        undo();
      }
      if ((e.metaKey || e.ctrlKey) && e.key === 'z' && e.shiftKey) {
        e.preventDefault();
        redo();
      }
      if (e.key === 'Escape') {
        if (validateCurrentField()) {
          setSelectedFieldIndex(null);
          setSelection(null);
          setValidationError(null);
        }
      }
    };
    window.addEventListener('keydown', handler);
    return () => window.removeEventListener('keydown', handler);
  }, [handleSave, undo, redo, validateCurrentField, showNewDialog, showBannerDialog, showHelp, pendingSwitch]);

  // --- Derived state ---

  const displayRows = useMemo(() => {
    if (!screen) return null;
    const rows = [...screen.rows];
    rows[23] = formatKeyLabelsRow(screen.keys || []);
    return rows;
  }, [screen, screen?.keys]);

  const screenPath = currentScreen ? `screens/${currentScreen}.screen` : null;
  const showingHelp = showHelp || !screen;
  const helpHighlight = showingHelp;

  const helpStatus = !screen
    ? 'Select a screen from the left or create a new one.'
    : 'F1 or Escape to close help';

  return (
    <div ref={appRef} style={{
      position: 'relative',
      display: 'inline-flex',
      flexDirection: 'column',
      backgroundColor: '#1a1a2e',
      color: '#e0e0e0',
      fontFamily: 'system-ui, sans-serif',
      minHeight: '100vh',
    }}>
      {showNewDialog && (
        <NewScreenDialog
          onConfirm={handleNewConfirm}
          onCancel={() => setShowNewDialog(false)}
        />
      )}
      {showBannerDialog && (
        <BannerDialog
          onConfirm={handleBannerConfirm}
          onCancel={() => setShowBannerDialog(false)}
        />
      )}
      {pendingSwitch && (
        <ConfirmDialog
          message="You have unsaved changes. Discard them and switch screens?"
          onConfirm={handleConfirmSwitch}
          onCancel={() => setPendingSwitch(null)}
        />
      )}
      <TitleBar screenName={currentScreen} screenPath={screenPath} />
      <div style={{ display: 'flex' }}>
        <ScreenList
          screens={screens}
          currentScreen={currentScreen}
          onSelect={handleSelectScreen}
          onNew={() => setShowNewDialog(true)}
        />
        <div style={{ display: 'flex', flexDirection: 'column' }}>
          <div style={{ padding: '12px' }}>
            {showingHelp ? (
              <HelpScreen statusText={helpStatus} />
            ) : (
              <ScreenGrid
                key={currentScreen}
                rows={displayRows!}
                fields={screen!.fields}
                selectedFieldIndex={selectedFieldIndex}
                selection={selection}
                overlay={bannerOverlay}
                onSelectField={handleSelectField}
                onSelectionChange={handleSelectionChange}
                onRowsChange={handleRowsChange}
                onOverlayClear={handleOverlayClear}
              />
            )}
          </div>
          <div style={{
            display: 'flex', alignItems: 'center', gap: '8px',
            padding: '6px 12px', backgroundColor: '#16213e', borderTop: '1px solid #333',
          }}>
            <button
              style={{
                padding: '4px 12px',
                backgroundColor: dirty ? '#50fa7b' : '#0f3460',
                border: dirty ? '1px solid #50fa7b' : '1px solid #444',
                borderRadius: '3px',
                color: dirty ? '#000' : '#e0e0e0',
                cursor: dirty ? 'pointer' : 'default',
                fontSize: '14px',
                fontWeight: dirty ? 'bold' : 'normal',
                opacity: screen && dirty ? 1 : 0.5,
              }}
              onClick={handleSave}
              disabled={!screen || !dirty}
            >
              Save
            </button>
            <button
              style={{
                padding: '4px 12px',
                backgroundColor: '#0f3460',
                border: '1px solid #444', borderRadius: '3px',
                color: '#e0e0e0',
                cursor: screen ? 'pointer' : 'default', fontSize: '14px',
                opacity: screen ? 1 : 0.5,
              }}
              onClick={() => screen && setShowBannerDialog(true)}
              disabled={!screen}
            >
              Banner
            </button>
            <button
              style={{
                padding: '4px 12px',
                backgroundColor: helpHighlight ? '#50fa7b' : '#0f3460',
                border: '1px solid #444', borderRadius: '3px',
                color: helpHighlight ? '#000' : '#e0e0e0',
                cursor: 'pointer', fontSize: '14px',
              }}
              onClick={() => setShowHelp(h => !h)}
            >
              Help
            </button>
          </div>
          <div style={{
            height: '100px', overflowY: 'auto', padding: '6px 12px',
            backgroundColor: '#0a0f1a', borderTop: '1px solid #333',
            fontFamily: 'monospace', fontSize: '13px', color: '#999',
          }}>
            {log.map((entry, i) => (
              <div key={i}>{entry}</div>
            ))}
            <div ref={logEndRef} />
          </div>
        </div>
        <div style={{ display: 'flex', flexDirection: 'column', flexShrink: 0, width: '320px', borderLeft: '1px solid #333' }}>
          <div style={{ flex: '1 1 0', overflowY: 'auto', minHeight: 0 }}>
            <AttributePanel
              field={screen && selectedFieldIndex !== null && selectedFieldIndex < screen.fields.length
                ? screen.fields[selectedFieldIndex] : null}
              selection={selection}
              canCreateField={canCreateField}
              validationError={validationError}
              maxRepeat={fieldMaxRepeat}
              onChange={(f) => { if (selectedFieldIndex !== null) handleFieldUpdate(selectedFieldIndex, f); }}
              onDelete={() => { if (selectedFieldIndex !== null) handleFieldDelete(selectedFieldIndex); }}
              onCreateField={handleCreateField}
            />
          </div>
          {screen && (
            <div style={{ flex: '0 0 auto', overflowY: 'auto', maxHeight: '40%' }}>
              <KeyActionsPanel
                keys={screen.keys || []}
                onChange={handleKeysChange}
              />
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
