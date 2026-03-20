import { useState } from 'react';
import { DefMenu, MenuItem } from '../types';

interface MenuEditorProps {
  menu: DefMenu;
  screens: string[];
  onChange: (menu: DefMenu) => void;
}

const inputStyle: React.CSSProperties = {
  padding: '3px 6px',
  backgroundColor: '#0f3460',
  border: '1px solid #444',
  borderRadius: '2px',
  color: '#e0e0e0',
  fontFamily: 'monospace',
  fontSize: '13px',
  boxSizing: 'border-box',
};

const smallBtnStyle: React.CSSProperties = {
  padding: '2px 8px',
  backgroundColor: '#333',
  border: '1px solid #555',
  borderRadius: '3px',
  color: '#ccc',
  cursor: 'pointer',
  fontSize: '12px',
  lineHeight: '1.4',
};

// --- Tree node component ---

interface TreeNodeProps {
  item: MenuItem;
  path: number[];
  depth: number;
  screens: string[];
  selectedPath: number[] | null;
  onSelect: (path: number[]) => void;
  onUpdate: (path: number[], item: MenuItem) => void;
  onRemove: (path: number[]) => void;
  onAddChild: (path: number[]) => void;
  onMoveUp: (path: number[]) => void;
  onMoveDown: (path: number[]) => void;
  isLast: boolean;
  isFirst: boolean;
}

function pathsEqual(a: number[] | null, b: number[]): boolean {
  if (!a) return false;
  return a.length === b.length && a.every((v, i) => v === b[i]);
}

function TreeNode({
  item, path, depth, screens, selectedPath,
  onSelect, onUpdate, onRemove, onAddChild, onMoveUp, onMoveDown,
  isFirst: _, isLast,
}: TreeNodeProps) {
  const isSelected = pathsEqual(selectedPath, path);
  const hasChildren = item.items && item.items.length > 0;
  const indent = depth * 24;

  return (
    <>
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          gap: '6px',
          padding: '4px 8px',
          paddingLeft: `${indent + 8}px`,
          backgroundColor: isSelected ? '#0f3460' : 'transparent',
          borderLeft: isSelected ? '3px solid #50fa7b' : '3px solid transparent',
          cursor: 'pointer',
          fontSize: '13px',
          minHeight: '28px',
        }}
        onClick={() => onSelect(path)}
      >
        {/* Tree connector */}
        {depth > 0 && (
          <span style={{ color: '#444', fontFamily: 'monospace', marginRight: '2px' }}>
            {isLast ? '\u2514' : '\u251C'}
          </span>
        )}
        {/* Key badge */}
        <span style={{
          backgroundColor: '#50fa7b', color: '#000',
          padding: '0 6px', borderRadius: '3px',
          fontWeight: 'bold', fontSize: '12px', minWidth: '20px', textAlign: 'center',
        }}>
          {item.key}
        </span>
        {/* Label */}
        <span style={{ color: '#e0e0e0', flex: 1 }}>
          {item.label || '(unnamed)'}
        </span>
        {/* Target indicator */}
        {item.screen && (
          <span style={{ color: '#888', fontSize: '11px' }}>
            {'\u2192'} {item.screen}
          </span>
        )}
        {hasChildren && (
          <span style={{ color: '#f1fa8c', fontSize: '11px' }}>
            {'\u25BC'} {item.items!.length} items
          </span>
        )}
      </div>
      {/* Render children */}
      {hasChildren && item.items!.map((child, i) => (
        <TreeNode
          key={i}
          item={child}
          path={[...path, i]}
          depth={depth + 1}
          screens={screens}
          selectedPath={selectedPath}
          onSelect={onSelect}
          onUpdate={onUpdate}
          onRemove={onRemove}
          onAddChild={onAddChild}
          onMoveUp={onMoveUp}
          onMoveDown={onMoveDown}
          isFirst={i === 0}
          isLast={i === item.items!.length - 1}
        />
      ))}
    </>
  );
}

// --- Helper: get/set item at path ---

function getItemAtPath(items: MenuItem[], path: number[]): MenuItem | null {
  if (path.length === 0) return null;
  let current = items[path[0]];
  for (let i = 1; i < path.length; i++) {
    if (!current?.items) return null;
    current = current.items[path[i]];
  }
  return current || null;
}

function updateItemAtPath(items: MenuItem[], path: number[], updater: (item: MenuItem) => MenuItem): MenuItem[] {
  if (path.length === 0) return items;
  const result = [...items];
  if (path.length === 1) {
    result[path[0]] = updater(result[path[0]]);
  } else {
    result[path[0]] = {
      ...result[path[0]],
      items: updateItemAtPath(result[path[0]].items || [], path.slice(1), updater),
    };
  }
  return result;
}

function removeItemAtPath(items: MenuItem[], path: number[]): MenuItem[] {
  if (path.length === 0) return items;
  if (path.length === 1) {
    return items.filter((_, i) => i !== path[0]);
  }
  const result = [...items];
  result[path[0]] = {
    ...result[path[0]],
    items: removeItemAtPath(result[path[0]].items || [], path.slice(1)),
  };
  return result;
}

function addChildAtPath(items: MenuItem[], path: number[]): MenuItem[] {
  const newItem: MenuItem = { key: '', label: '', description: '' };
  if (path.length === 0) {
    return [...items, newItem];
  }
  return updateItemAtPath(items, path, item => ({
    ...item,
    items: [...(item.items || []), newItem],
  }));
}

function getParentItems(items: MenuItem[], path: number[]): MenuItem[] {
  if (path.length <= 1) return items;
  const parent = getItemAtPath(items, path.slice(0, -1));
  return parent?.items || [];
}

function swapItems(items: MenuItem[], path: number[], direction: -1 | 1): MenuItem[] {
  const idx = path[path.length - 1];
  const parentPath = path.slice(0, -1);
  const siblings = parentPath.length === 0 ? items : (getItemAtPath(items, parentPath)?.items || []);
  const newIdx = idx + direction;
  if (newIdx < 0 || newIdx >= siblings.length) return items;

  const newSiblings = [...siblings];
  [newSiblings[idx], newSiblings[newIdx]] = [newSiblings[newIdx], newSiblings[idx]];

  if (parentPath.length === 0) return newSiblings;
  return updateItemAtPath(items, parentPath, item => ({ ...item, items: newSiblings }));
}

// --- Main editor ---

export default function MenuEditor({ menu, screens, onChange }: MenuEditorProps) {
  const [selectedPath, setSelectedPath] = useState<number[] | null>(null);

  const selectedItem = selectedPath ? getItemAtPath(menu.items, selectedPath) : null;
  const siblings = selectedPath ? getParentItems(menu.items, selectedPath) : [];
  const selectedIdx = selectedPath ? selectedPath[selectedPath.length - 1] : -1;

  const handleUpdate = (path: number[], item: MenuItem) => {
    onChange({ ...menu, items: updateItemAtPath(menu.items, path, () => item) });
  };

  const handleRemove = (path: number[]) => {
    onChange({ ...menu, items: removeItemAtPath(menu.items, path) });
    setSelectedPath(null);
  };

  const handleAddChild = (path: number[]) => {
    onChange({ ...menu, items: addChildAtPath(menu.items, path) });
  };

  const handleAddTopLevel = () => {
    onChange({ ...menu, items: addChildAtPath(menu.items, []) });
  };

  const handleMoveUp = (path: number[]) => {
    onChange({ ...menu, items: swapItems(menu.items, path, -1) });
    const newPath = [...path];
    newPath[newPath.length - 1]--;
    setSelectedPath(newPath);
  };

  const handleMoveDown = (path: number[]) => {
    onChange({ ...menu, items: swapItems(menu.items, path, 1) });
    const newPath = [...path];
    newPath[newPath.length - 1]++;
    setSelectedPath(newPath);
  };

  return (
    <div style={{ display: 'flex', gap: '0', minWidth: '700px' }}>
      {/* Tree panel */}
      <div style={{
        flex: '1 1 auto',
        backgroundColor: '#1a1a2e',
        borderRight: '1px solid #333',
        minHeight: '400px',
      }}>
        {/* Menu header */}
        <div style={{
          padding: '10px 12px',
          borderBottom: '1px solid #333',
          display: 'flex', gap: '8px', alignItems: 'center',
        }}>
          <span style={{ color: '#888', fontSize: '13px' }}>Title:</span>
          <input
            style={{ ...inputStyle, flex: 1 }}
            value={menu.title}
            onChange={e => onChange({ ...menu, title: e.target.value })}
            placeholder="Menu title"
          />
        </div>
        {/* Tree */}
        <div style={{ padding: '4px 0' }}>
          {menu.items.length === 0 ? (
            <div style={{ padding: '20px', color: '#666', textAlign: 'center', fontSize: '13px' }}>
              No menu items. Click "Add Item" to start.
            </div>
          ) : (
            menu.items.map((item, i) => (
              <TreeNode
                key={i}
                item={item}
                path={[i]}
                depth={0}
                screens={screens}
                selectedPath={selectedPath}
                onSelect={setSelectedPath}
                onUpdate={handleUpdate}
                onRemove={handleRemove}
                onAddChild={handleAddChild}
                onMoveUp={handleMoveUp}
                onMoveDown={handleMoveDown}
                isFirst={i === 0}
                isLast={i === menu.items.length - 1}
              />
            ))
          )}
        </div>
        {/* Add top-level button */}
        <div style={{ padding: '8px 12px' }}>
          <button
            style={{ ...smallBtnStyle, backgroundColor: '#50fa7b', color: '#000', fontWeight: 'bold', width: '100%', padding: '6px' }}
            onClick={handleAddTopLevel}
          >
            Add Item
          </button>
        </div>
      </div>

      {/* Detail panel */}
      <div style={{
        width: '280px', flexShrink: 0,
        backgroundColor: '#16213e',
        padding: '12px',
        fontSize: '13px',
      }}>
        {selectedItem ? (
          <>
            <div style={{ marginBottom: '10px', fontWeight: 'bold', color: '#50fa7b' }}>
              Item Properties
            </div>
            <div style={{ marginBottom: '6px' }}>
              <div style={{ color: '#888', fontSize: '11px', marginBottom: '2px' }}>Key</div>
              <input
                style={{ ...inputStyle, width: '60px' }}
                value={selectedItem.key}
                onChange={e => handleUpdate(selectedPath!, { ...selectedItem, key: e.target.value })}
              />
            </div>
            <div style={{ marginBottom: '6px' }}>
              <div style={{ color: '#888', fontSize: '11px', marginBottom: '2px' }}>Label</div>
              <input
                style={{ ...inputStyle, width: '100%' }}
                value={selectedItem.label}
                onChange={e => handleUpdate(selectedPath!, { ...selectedItem, label: e.target.value })}
              />
            </div>
            <div style={{ marginBottom: '6px' }}>
              <div style={{ color: '#888', fontSize: '11px', marginBottom: '2px' }}>Description</div>
              <input
                style={{ ...inputStyle, width: '100%' }}
                value={selectedItem.description}
                onChange={e => handleUpdate(selectedPath!, { ...selectedItem, description: e.target.value })}
              />
            </div>
            <div style={{ marginBottom: '10px' }}>
              <div style={{ color: '#888', fontSize: '11px', marginBottom: '2px' }}>Target Screen</div>
              <select
                style={{ ...inputStyle, width: '100%' }}
                value={selectedItem.screen || ''}
                onChange={e => handleUpdate(selectedPath!, {
                  ...selectedItem,
                  screen: e.target.value || undefined,
                })}
              >
                <option value="">-- none (submenu only) --</option>
                {screens.map(s => <option key={s} value={s}>{s}</option>)}
              </select>
            </div>

            <hr style={{ borderColor: '#333', margin: '10px 0' }} />

            {/* Action buttons */}
            <div style={{ display: 'flex', flexDirection: 'column', gap: '6px' }}>
              <button
                style={{ ...smallBtnStyle, backgroundColor: '#0f3460' }}
                onClick={() => handleAddChild(selectedPath!)}
              >
                Add Sub-Item
              </button>
              <div style={{ display: 'flex', gap: '6px' }}>
                <button
                  style={{ ...smallBtnStyle, flex: 1, opacity: selectedIdx > 0 ? 1 : 0.4 }}
                  disabled={selectedIdx <= 0}
                  onClick={() => handleMoveUp(selectedPath!)}
                >
                  Move Up
                </button>
                <button
                  style={{ ...smallBtnStyle, flex: 1, opacity: selectedIdx < siblings.length - 1 ? 1 : 0.4 }}
                  disabled={selectedIdx >= siblings.length - 1}
                  onClick={() => handleMoveDown(selectedPath!)}
                >
                  Move Down
                </button>
              </div>
              <button
                style={{ ...smallBtnStyle, backgroundColor: '#ff5555', color: '#fff', marginTop: '8px' }}
                onClick={() => handleRemove(selectedPath!)}
              >
                Delete Item
              </button>
            </div>
          </>
        ) : (
          <div style={{ color: '#666', textAlign: 'center', marginTop: '40px' }}>
            Select an item to edit its properties
          </div>
        )}
      </div>
    </div>
  );
}
