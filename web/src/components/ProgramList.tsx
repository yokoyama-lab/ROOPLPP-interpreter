import React from 'react';
import Button from '@material-ui/core/Button';
import Menu from '@material-ui/core/Menu';
import MenuItem from '@material-ui/core/MenuItem';

interface ProgramListProps {
  onClick: (algorithmSrc: string) => void
}

// サンプルプログラムを追加するときは、
// {algorithmName: ページ上での表示名, algorithmSrc: サーバ上でのファイル名}
// を下記配列に追加
const PROGRAME_EXAMPLE_LIST: { algorithmName: string, algorithmSrc: string }[] = [
  { algorithmName: 'Fibonacci', algorithmSrc: 'fib.rplpp' },
  { algorithmName: 'Square root', algorithmSrc: 'sqrt.rplpp' },
  { algorithmName: 'Factorization', algorithmSrc: 'factor.rplpp' },
  { algorithmName: 'Perm-to-code', algorithmSrc: 'perm-to-code.rplpp' },
  { algorithmName: 'LinkedList', algorithmSrc: 'LinkedList.rplpp' },
  { algorithmName: 'DoublyLinkedList', algorithmSrc: 'DoublyLinkedList.rplpp' },
  { algorithmName: 'BinaryTree', algorithmSrc: 'BinaryTree.rplpp' },
  { algorithmName: 'BinaryTree_print', algorithmSrc: 'BinaryTree_print.rplpp' },
];

// プログラム例の一覧
export default function ProgramList(props: ProgramListProps) {
  const [anchorEl, setAnchorEl] = React.useState<null | HTMLElement>(null);

  const handleClick = (event: React.MouseEvent<HTMLButtonElement>) => {
    setAnchorEl(event.currentTarget);
  };

  const handleClose = () => {
    setAnchorEl(null);
  };

  const menuItems = PROGRAME_EXAMPLE_LIST.map((programExample) =>
    <MenuItem
      key={programExample.algorithmName}
      onClick={() => {
        props.onClick(programExample.algorithmSrc)
        handleClose()
      }}
    >
      {programExample.algorithmName}
    </MenuItem>
  );
  return (
    <>
      <div>
        <Button aria-controls="Example-menu" aria-haspopup="true" onClick={handleClick}>
          Example
        </Button>
        <Menu
          id="Example-menu"
          anchorEl={anchorEl}
          keepMounted
          open={Boolean(anchorEl)}
          onClose={handleClose}
        >
          {menuItems}
        </Menu>
      </div>
    </>
  );
}