export type Color = 'default' | 'blue' | 'red' | 'pink' | 'green' | 'turquoise' | 'yellow' | 'white';
export type Highlighting = 'default' | 'blink' | 'reverse-video' | 'underscore';

export interface Field {
  fromRow: number;
  fromCol: number;
  len: number;
  name: string | null;
  anonymous: boolean;
  write: boolean;
  autoskip: boolean;
  intense: boolean;
  hidden: boolean;
  numericOnly: boolean;
  color: Color;
  highlighting: Highlighting;
  keepspaces: boolean;
  positionOnly: boolean;
  default: boolean;
  transient: boolean;
  repeat: number;
}

export interface Selection {
  fromRow: number;
  toRow: number;
  fromCol: number;
  toCol: number;
}

export type AidKey = 'enter' | 'clear' | 'pa1' | 'pa2' | 'pa3'
  | 'pf1' | 'pf2' | 'pf3' | 'pf4' | 'pf5' | 'pf6' | 'pf7' | 'pf8' | 'pf9'
  | 'pf10' | 'pf11' | 'pf12' | 'pf13' | 'pf14' | 'pf15' | 'pf16' | 'pf17'
  | 'pf18' | 'pf19' | 'pf20' | 'pf21' | 'pf22' | 'pf23' | 'pf24';

export type KeyActionType = 'handler' | 'back' | 'goto';

export interface KeyAction {
  aidKey: AidKey;
  label: string;
  action: KeyActionType;
  gotoScreen?: string;
  hidden?: boolean;
}

export const AID_KEYS: AidKey[] = [
  'enter', 'clear', 'pa1', 'pa2', 'pa3',
  'pf1', 'pf2', 'pf3', 'pf4', 'pf5', 'pf6', 'pf7', 'pf8', 'pf9',
  'pf10', 'pf11', 'pf12', 'pf13', 'pf14', 'pf15', 'pf16', 'pf17',
  'pf18', 'pf19', 'pf20', 'pf21', 'pf22', 'pf23', 'pf24',
];

export interface DynamicArea {
  name: string;
  fromRow: number;
  fromCol: number;
  toRow: number;
  toCol: number;
}

export interface DefScreen {
  name: string;
  rows: string[];
  fields: Field[];
  keys?: KeyAction[];
  dynamicAreas?: DynamicArea[];
  noCommand?: boolean;
  command?: boolean;
  anonymous?: boolean;
  menu?: string;
  aliases?: string[];
}

export interface MenuItem {
  key: string;
  label: string;
  description: string;
  screen?: string;
  items?: MenuItem[];
}

export interface DefMenu {
  name: string;
  title: string;
  items: MenuItem[];
}

export function makeDefaultField(fromRow: number, fromCol: number, len: number): Field {
  return {
    fromRow,
    fromCol,
    len,
    name: null,
    anonymous: false,
    write: false,
    autoskip: false,
    intense: false,
    hidden: false,
    numericOnly: false,
    color: 'default',
    highlighting: 'default',
    keepspaces: false,
    positionOnly: false,
    default: false,
    transient: false,
    repeat: 1,
  };
}
