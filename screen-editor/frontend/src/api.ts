import { DefScreen, DefMenu } from './types';

export async function listScreens(): Promise<string[]> {
  const resp = await fetch('/api/screens');
  if (!resp.ok) {
    const err = await resp.json();
    throw new Error(err.error || 'Failed to list screens');
  }
  const data = await resp.json();
  return data.screens;
}

export async function loadScreen(name: string): Promise<DefScreen> {
  const resp = await fetch(`/api/screen?name=${encodeURIComponent(name)}`);
  if (!resp.ok) {
    const err = await resp.json();
    throw new Error(err.error || 'Failed to load screen');
  }
  return resp.json();
}

export async function saveScreen(screen: DefScreen): Promise<void> {
  const resp = await fetch('/api/screen', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(screen),
  });
  if (!resp.ok) {
    const err = await resp.json();
    throw new Error(err.error || 'Failed to save screen');
  }
}

export async function listMenus(): Promise<string[]> {
  const resp = await fetch('/api/menus');
  if (!resp.ok) {
    const err = await resp.json();
    throw new Error(err.error || 'Failed to list menus');
  }
  const data = await resp.json();
  return data.menus;
}

export async function loadMenu(name: string): Promise<DefMenu> {
  const resp = await fetch(`/api/menu?name=${encodeURIComponent(name)}`);
  if (!resp.ok) {
    const err = await resp.json();
    throw new Error(err.error || 'Failed to load menu');
  }
  return resp.json();
}

export async function saveMenu(menu: DefMenu): Promise<void> {
  const resp = await fetch('/api/menu', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(menu),
  });
  if (!resp.ok) {
    const err = await resp.json();
    throw new Error(err.error || 'Failed to save menu');
  }
}
