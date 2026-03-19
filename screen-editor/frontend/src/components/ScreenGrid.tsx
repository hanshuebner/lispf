import { useRef, useState, useEffect, useCallback, useMemo } from 'react';
import { Field, Selection } from '../types';
import FieldOverlay from './FieldOverlay';
import './ScreenGrid.css';

interface ScreenGridProps {
  rows: string[];
  fields: Field[];
  selectedFieldIndex: number | null;
  selection: Selection | null;
  overlay: string[] | null;
  onSelectField: (index: number | null) => void;
  onSelectionChange: (sel: Selection | null) => void;
  onRowsChange: (rows: string[]) => void;
  onOverlayClear: () => void;
}

const ROWS = 24;
const COLS = 80;
const LOCKED_ROWS = new Set([0, 22, 23]);
const MIN_APP_ROW = 1;
const MAX_APP_ROW = 21;

function isAppRow(row: number) {
  return row >= MIN_APP_ROW && row <= MAX_APP_ROW;
}

function clampRow(row: number) {
  return Math.max(MIN_APP_ROW, Math.min(MAX_APP_ROW, row));
}

export default function ScreenGrid({
  rows, fields, selectedFieldIndex, selection, overlay,
  onSelectField, onSelectionChange, onRowsChange, onOverlayClear,
}: ScreenGridProps) {
  const containerRef = useRef<HTMLDivElement>(null);
  const measureRef = useRef<HTMLSpanElement>(null);
  const [cellSize, setCellSize] = useState({ w: 9.6, h: 19.2 });
  const [cursor, setCursor] = useState<{ row: number; col: number } | null>(null);
  const [dragStart, setDragStart] = useState<{ row: number; col: number } | null>(null);
  const [dragEnd, setDragEnd] = useState<{ row: number; col: number } | null>(null);
  const [dragging, setDragging] = useState(false);
  const [moveOffset, setMoveOffset] = useState<{ dRow: number; dCol: number }>({ dRow: 0, dCol: 0 });
  const [overlayPos, setOverlayPos] = useState<{ row: number; col: number } | null>(null);

  // Initialize overlay position when overlay appears
  useEffect(() => {
    if (overlay) {
      const overlayWidth = Math.max(0, ...overlay.map(l => l.length));
      const startRow = cursor ? cursor.row : Math.max(MIN_APP_ROW, Math.floor((MIN_APP_ROW + MAX_APP_ROW - overlay.length) / 2));
      const startCol = cursor ? cursor.col : Math.max(0, Math.floor((COLS - overlayWidth) / 2));
      setOverlayPos({ row: Math.max(MIN_APP_ROW, Math.min(MAX_APP_ROW - overlay.length + 1, startRow)), col: Math.max(0, Math.min(COLS - overlayWidth, startCol)) });
      onSelectionChange(null);
      setCursor(null);
    } else {
      setOverlayPos(null);
    }
  }, [overlay]);

  useEffect(() => {
    if (measureRef.current) {
      const rect = measureRef.current.getBoundingClientRect();
      if (rect.width > 0 && rect.height > 0) {
        setCellSize({ w: rect.width, h: rect.height });
      }
    }
  }, []);

  // Reset move offset when selection changes
  useEffect(() => {
    setMoveOffset({ dRow: 0, dCol: 0 });
  }, [selection]);

  const isMoving = selection && (moveOffset.dRow !== 0 || moveOffset.dCol !== 0);

  const posFromEvent = useCallback((e: React.MouseEvent): { row: number; col: number } | null => {
    const container = containerRef.current;
    if (!container) return null;
    const rect = container.getBoundingClientRect();
    const x = e.clientX - rect.left;
    const y = e.clientY - rect.top;
    const col = Math.min(COLS - 1, Math.max(0, Math.floor(x / cellSize.w)));
    const row = Math.min(ROWS - 1, Math.max(0, Math.floor(y / cellSize.h)));
    return { row, col };
  }, [cellSize]);

  const handleMouseDown = useCallback((e: React.MouseEvent) => {
    const pos = posFromEvent(e);
    if (!pos || !isAppRow(pos.row)) return;

    setDragStart(pos);
    setDragEnd(pos);
    setDragging(false);
    setCursor(pos);
    onSelectionChange(null);
  }, [posFromEvent, onSelectionChange]);

  const handleMouseMove = useCallback((e: React.MouseEvent) => {
    if (!dragStart) return;
    const pos = posFromEvent(e);
    if (!pos) return;
    setDragEnd({ row: clampRow(pos.row), col: pos.col });
    setDragging(true);
  }, [dragStart, posFromEvent]);

  const handleMouseUp = useCallback(() => {
    if (!dragStart || !dragEnd) return;

    if (dragging) {
      const fromRow = Math.min(dragStart.row, dragEnd.row);
      const toRow = Math.max(dragStart.row, dragEnd.row);
      const fromCol = Math.min(dragStart.col, dragEnd.col);
      const toCol = Math.max(dragStart.col, dragEnd.col);
      if (toCol > fromCol || toRow > fromRow) {
        onSelectionChange({ fromRow, toRow, fromCol, toCol });
        onSelectField(null);
      }
    } else {
      const pos = dragStart;
      const fieldIndex = fields.findIndex(f => {
        const repeat = f.repeat || 1;
        return pos.row >= f.fromRow && pos.row < f.fromRow + repeat
          && pos.col >= f.fromCol && pos.col < f.fromCol + f.len;
      });
      onSelectField(fieldIndex >= 0 ? fieldIndex : null);
    }

    setDragStart(null);
    setDragEnd(null);
    setDragging(false);
  }, [dragStart, dragEnd, dragging, fields, onSelectField, onSelectionChange]);

  // Apply the pending move: clear source, place at destination
  const applyMove = useCallback(() => {
    if (!selection || !isMoving) return;
    const sel = selection;
    const { dRow, dCol } = moveOffset;
    const newRows = rows.map(r => r.padEnd(COLS, ' ').substring(0, COLS));

    // Extract content from original position
    const content: string[] = [];
    for (let r = sel.fromRow; r <= sel.toRow; r++) {
      content.push(newRows[r].substring(sel.fromCol, sel.toCol + 1));
    }

    // Clear original position
    for (let r = sel.fromRow; r <= sel.toRow; r++) {
      newRows[r] = newRows[r].substring(0, sel.fromCol)
        + ' '.repeat(sel.toCol - sel.fromCol + 1)
        + newRows[r].substring(sel.toCol + 1);
    }

    // Place at destination
    for (let i = 0; i < content.length; i++) {
      const r = sel.fromRow + dRow + i;
      const c = sel.fromCol + dCol;
      newRows[r] = newRows[r].substring(0, c)
        + content[i]
        + newRows[r].substring(c + content[i].length);
    }

    onRowsChange(newRows);
    onSelectionChange({
      fromRow: sel.fromRow + dRow,
      toRow: sel.toRow + dRow,
      fromCol: sel.fromCol + dCol,
      toCol: sel.toCol + dCol,
    });
  }, [selection, moveOffset, isMoving, rows, onRowsChange, onSelectionChange]);

  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if (document.activeElement && document.activeElement !== document.body) return;

      // Overlay placement mode
      if (overlay && overlayPos) {
        const overlayWidth = Math.max(0, ...overlay.map(l => l.length));
        if (['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight'].includes(e.key)) {
          e.preventDefault();
          setOverlayPos(prev => {
            if (!prev) return prev;
            const dRow = e.key === 'ArrowUp' ? -1 : e.key === 'ArrowDown' ? 1 : 0;
            const dCol = e.key === 'ArrowLeft' ? -1 : e.key === 'ArrowRight' ? 1 : 0;
            const newRow = prev.row + dRow;
            const newCol = prev.col + dCol;
            if (newRow < MIN_APP_ROW || newRow + overlay.length - 1 > MAX_APP_ROW) return prev;
            if (newCol < 0 || newCol + overlayWidth > COLS) return prev;
            return { row: newRow, col: newCol };
          });
          return;
        }
        if (e.key === 'Enter') {
          e.preventDefault();
          // Apply overlay transparently (spaces don't overwrite)
          const newRows = rows.map(r => r.padEnd(COLS, ' ').substring(0, COLS));
          for (let i = 0; i < overlay.length; i++) {
            const r = overlayPos.row + i;
            if (r > MAX_APP_ROW) break;
            const rowChars = newRows[r].split('');
            for (let j = 0; j < overlay[i].length; j++) {
              const c = overlayPos.col + j;
              if (c >= COLS) break;
              if (overlay[i][j] !== ' ') {
                rowChars[c] = overlay[i][j];
              }
            }
            newRows[r] = rowChars.join('');
          }
          onRowsChange(newRows);
          onOverlayClear();
          return;
        }
        if (e.key === 'Escape') {
          e.preventDefault();
          onOverlayClear();
          return;
        }
        return; // block all other keys during overlay
      }

      // Selection movement mode
      if (selection && ['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight'].includes(e.key)) {
        e.preventDefault();
        const dRow = e.key === 'ArrowUp' ? -1 : e.key === 'ArrowDown' ? 1 : 0;
        const dCol = e.key === 'ArrowLeft' ? -1 : e.key === 'ArrowRight' ? 1 : 0;
        setMoveOffset(prev => {
          const newDRow = prev.dRow + dRow;
          const newDCol = prev.dCol + dCol;
          // Bounds check
          if (selection.fromRow + newDRow < MIN_APP_ROW || selection.toRow + newDRow > MAX_APP_ROW) return prev;
          if (selection.fromCol + newDCol < 0 || selection.toCol + newDCol >= COLS) return prev;
          return { dRow: newDRow, dCol: newDCol };
        });
        return;
      }

      // Delete/Backspace clears selection content
      if (selection && (e.key === 'Backspace' || e.key === 'Delete')) {
        e.preventDefault();
        const newRows = rows.map(r => r.padEnd(COLS, ' ').substring(0, COLS));
        for (let r = selection.fromRow; r <= selection.toRow; r++) {
          newRows[r] = newRows[r].substring(0, selection.fromCol)
            + ' '.repeat(selection.toCol - selection.fromCol + 1)
            + newRows[r].substring(selection.toCol + 1);
        }
        onRowsChange(newRows);
        onSelectionChange(null);
        return;
      }

      // Enter finalizes the move
      if (selection && e.key === 'Enter') {
        e.preventDefault();
        if (isMoving) {
          applyMove();
        } else {
          onSelectionChange(null);
        }
        return;
      }

      // Copy selection to clipboard
      if (selection && (e.metaKey || e.ctrlKey) && e.key === 'c') {
        e.preventDefault();
        const padded = rows.map(r => r.padEnd(COLS, ' ').substring(0, COLS));
        const lines: string[] = [];
        for (let r = selection.fromRow; r <= selection.toRow; r++) {
          lines.push(padded[r].substring(selection.fromCol, selection.toCol + 1).trimEnd());
        }
        navigator.clipboard.writeText(lines.join('\n'));
        return;
      }

      // Cut selection to clipboard
      if (selection && (e.metaKey || e.ctrlKey) && e.key === 'x') {
        e.preventDefault();
        const padded = rows.map(r => r.padEnd(COLS, ' ').substring(0, COLS));
        const lines: string[] = [];
        for (let r = selection.fromRow; r <= selection.toRow; r++) {
          lines.push(padded[r].substring(selection.fromCol, selection.toCol + 1).trimEnd());
        }
        navigator.clipboard.writeText(lines.join('\n'));
        // Clear the selection area
        const newRows = [...padded];
        for (let r = selection.fromRow; r <= selection.toRow; r++) {
          newRows[r] = newRows[r].substring(0, selection.fromCol)
            + ' '.repeat(selection.toCol - selection.fromCol + 1)
            + newRows[r].substring(selection.toCol + 1);
        }
        onRowsChange(newRows);
        onSelectionChange(null);
        return;
      }

      // Paste from clipboard
      if ((e.metaKey || e.ctrlKey) && e.key === 'v') {
        e.preventDefault();
        const pos = cursor || (selection ? { row: selection.fromRow, col: selection.fromCol } : null);
        if (!pos || !isAppRow(pos.row)) return;
        navigator.clipboard.readText().then(text => {
          const lines = text.split('\n');
          const newRows = rows.map(r => r.padEnd(COLS, ' ').substring(0, COLS));
          for (let i = 0; i < lines.length; i++) {
            const r = pos.row + i;
            if (!isAppRow(r)) break;
            const line = lines[i].substring(0, COLS - pos.col);
            newRows[r] = newRows[r].substring(0, pos.col)
              + line
              + newRows[r].substring(pos.col + line.length);
          }
          onRowsChange(newRows);
          if (lines.length === 1) {
            setCursor({ row: pos.row, col: Math.min(COLS - 1, pos.col + lines[0].length) });
          }
          onSelectionChange(null);
        });
        return;
      }

      if (e.key === 'Escape') {
        if (selection) {
          onSelectionChange(null);
          return;
        }
        onSelectField(null);
        setCursor(null);
        return;
      }

      if (!cursor) return;
      const { row, col } = cursor;

      if (e.key === 'ArrowLeft') {
        e.preventDefault();
        setCursor({ row, col: Math.max(0, col - 1) });
      } else if (e.key === 'ArrowRight') {
        e.preventDefault();
        setCursor({ row, col: Math.min(COLS - 1, col + 1) });
      } else if (e.key === 'ArrowUp') {
        e.preventDefault();
        let newRow = row - 1;
        while (newRow >= 0 && LOCKED_ROWS.has(newRow)) newRow--;
        if (newRow >= 0) setCursor({ row: newRow, col });
      } else if (e.key === 'ArrowDown') {
        e.preventDefault();
        let newRow = row + 1;
        while (newRow < ROWS && LOCKED_ROWS.has(newRow)) newRow++;
        if (newRow < ROWS) setCursor({ row: newRow, col });
      } else if (e.key === 'Backspace' || e.key === 'Delete') {
        e.preventDefault();
        if (!isAppRow(row) || col === 0) return;
        const newRows = [...rows];
        const rowStr = newRows[row].padEnd(COLS, ' ');
        newRows[row] = rowStr.substring(0, col - 1) + ' ' + rowStr.substring(col);
        onRowsChange(newRows);
        setCursor({ row, col: col - 1 });
      } else if (e.key.length === 1 && !e.ctrlKey && !e.metaKey) {
        e.preventDefault();
        if (!isAppRow(row)) return;
        const newRows = [...rows];
        const rowStr = newRows[row].padEnd(COLS, ' ');
        newRows[row] = rowStr.substring(0, col) + e.key + rowStr.substring(col + 1);
        onRowsChange(newRows);
        setCursor({ row, col: Math.min(COLS - 1, col + 1) });
      }
    };
    window.addEventListener('keydown', handler);
    return () => window.removeEventListener('keydown', handler);
  }, [cursor, selection, moveOffset, isMoving, overlay, overlayPos, rows, onRowsChange, onSelectionChange, onSelectField, onOverlayClear, applyMove]);

  // Compute display rows, applying overlay or move preview if active
  const displayRows = useMemo(() => {
    const base = rows.map(r => r.padEnd(COLS, ' ').substring(0, COLS));

    // Overlay preview (transparent - spaces don't overwrite)
    if (overlay && overlayPos) {
      const preview = base.map(r => r.split(''));
      for (let i = 0; i < overlay.length; i++) {
        const r = overlayPos.row + i;
        if (r > MAX_APP_ROW) break;
        for (let j = 0; j < overlay[i].length; j++) {
          const c = overlayPos.col + j;
          if (c >= COLS) break;
          if (overlay[i][j] !== ' ') {
            preview[r][c] = overlay[i][j];
          }
        }
      }
      return preview.map(chars => chars.join(''));
    }

    if (!selection || !isMoving) return base;

    const sel = selection;
    const { dRow, dCol } = moveOffset;
    const preview = base.map(r => r.split(''));

    // Extract content from source
    const content: string[][] = [];
    for (let r = sel.fromRow; r <= sel.toRow; r++) {
      content.push(preview[r].slice(sel.fromCol, sel.toCol + 1));
      // Clear source
      for (let c = sel.fromCol; c <= sel.toCol; c++) {
        preview[r][c] = ' ';
      }
    }

    // Place at destination
    for (let i = 0; i < content.length; i++) {
      const r = sel.fromRow + dRow + i;
      for (let j = 0; j < content[i].length; j++) {
        preview[r][sel.fromCol + dCol + j] = content[i][j];
      }
    }

    return preview.map(chars => chars.join(''));
  }, [rows, selection, isMoving, moveOffset, overlay, overlayPos]);

  // In-progress drag selection
  const dragSel = dragging && dragStart && dragEnd
    ? {
        fromRow: Math.min(dragStart.row, dragEnd.row),
        toRow: Math.max(dragStart.row, dragEnd.row),
        fromCol: Math.min(dragStart.col, dragEnd.col),
        toCol: Math.max(dragStart.col, dragEnd.col),
      }
    : null;

  // The committed selection (original position)
  const selRect = dragSel || selection;

  // The destination rectangle (offset from original)
  const destRect = selection && isMoving
    ? {
        fromRow: selection.fromRow + moveOffset.dRow,
        toRow: selection.toRow + moveOffset.dRow,
        fromCol: selection.fromCol + moveOffset.dCol,
        toCol: selection.toCol + moveOffset.dCol,
      }
    : null;

  // Overlay rectangle
  const overlayRect = overlay && overlayPos ? {
    fromRow: overlayPos.row,
    toRow: overlayPos.row + overlay.length - 1,
    fromCol: overlayPos.col,
    toCol: overlayPos.col + Math.max(0, ...overlay.map(l => l.length)) - 1,
  } : null;

  const statusLeft = overlay
    ? 'Arrows to position, Enter to place, Escape to cancel'
    : selection
      ? isMoving
        ? 'Enter to confirm move, Escape to cancel'
        : 'Arrows to move, Delete to clear, Enter to deselect, Escape to cancel'
      : cursor
        ? 'Type to enter text, drag to select, click a field to edit'
        : 'Click to place cursor, drag to select, click a field to edit';
  const statusRight = cursor ? `Row ${cursor.row} Col ${cursor.col}` : '';

  return (
    <div style={{ display: 'inline-block' }}>
      <span
        ref={measureRef}
        style={{
          position: 'absolute',
          visibility: 'hidden',
          fontFamily: "'3270', 'IBM Plex Mono', 'Courier New', monospace",
          fontSize: '16px',
          lineHeight: '1.2',
          whiteSpace: 'pre',
        }}
      >
        X
      </span>

      <div
        ref={containerRef}
        className="screen-grid-container"
        onMouseDown={handleMouseDown}
        onMouseMove={handleMouseMove}
        onMouseUp={handleMouseUp}
      >
        <pre className="screen-grid-pre">
          {displayRows.map((row, i) => (
            <span key={i} className={LOCKED_ROWS.has(i) ? 'locked-row' : undefined}>
              {row}{i < displayRows.length - 1 ? '\n' : ''}
            </span>
          ))}
        </pre>

        <div className="field-overlays">
          {fields.map((field, i) => (
            <FieldOverlay
              key={i}
              field={field}
              index={i}
              cellWidth={cellSize.w}
              cellHeight={cellSize.h}
              selected={i === selectedFieldIndex}
              onClick={onSelectField}
            />
          ))}
        </div>

        {cursor && !selRect && (
          <div
            className="screen-grid-cursor"
            style={{
              left: cursor.col * cellSize.w,
              top: cursor.row * cellSize.h,
              width: cellSize.w,
              height: cellSize.h,
            }}
          />
        )}

        {/* Original selection position */}
        {selRect && (
          <div
            className={destRect ? 'screen-grid-selection-source' : 'screen-grid-drag-selection'}
            style={{
              left: selRect.fromCol * cellSize.w,
              top: selRect.fromRow * cellSize.h,
              width: (selRect.toCol - selRect.fromCol + 1) * cellSize.w,
              height: (selRect.toRow - selRect.fromRow + 1) * cellSize.h,
            }}
          />
        )}

        {/* Overlay placement preview */}
        {overlayRect && (
          <div
            className="screen-grid-drag-selection"
            style={{
              left: overlayRect.fromCol * cellSize.w,
              top: overlayRect.fromRow * cellSize.h,
              width: (overlayRect.toCol - overlayRect.fromCol + 1) * cellSize.w,
              height: (overlayRect.toRow - overlayRect.fromRow + 1) * cellSize.h,
            }}
          />
        )}

        {/* Move destination preview */}
        {destRect && (
          <div
            className="screen-grid-drag-selection"
            style={{
              left: destRect.fromCol * cellSize.w,
              top: destRect.fromRow * cellSize.h,
              width: (destRect.toCol - destRect.fromCol + 1) * cellSize.w,
              height: (destRect.toRow - destRect.fromRow + 1) * cellSize.h,
            }}
          />
        )}
      </div>
      <div className="screen-grid-status">
        <span>{statusLeft}</span>
        <span>{statusRight}</span>
      </div>
    </div>
  );
}
