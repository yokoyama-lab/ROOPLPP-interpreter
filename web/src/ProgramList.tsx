import React from 'react';

interface ProgramListProps {
  onClick: (algorithmSrc: string) => void
}

// サンプルプログラムを追加するときは、
// {algorithmName: ページ上での表示名, algorithmSrc: サーバ上でのファイル名}
// を下記配列に追加
const PROGRAME_EXAMPLE_LIST: {algorithmName: string, algorithmSrc: string}[] = [
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
  const listItems = PROGRAME_EXAMPLE_LIST.map((programExample) =>
    <li
      onClick={() => props.onClick(programExample.algorithmSrc)}>
      {programExample.algorithmName}
    </li>
  );
  return (
    <ul>{listItems}</ul>
  );
}