import { Ace } from 'ace-builds';
import React, { useEffect, useState } from 'react';
import AceEditor from 'react-ace';

// ace-editorのrooplpp用シンタックスハイライター
//import "../mode-rooplpp";
import "ace-builds/src-min-noconflict/mode-c_cpp";

// ace-editorのテーマ
import "ace-builds/src-noconflict/theme-tomorrow";

type Props = {
  isResultActive: boolean
  program: string
  onChange: (program: string) => void
}

export default function Editor(props: Props) {
  const [editor, setEditor] = useState<Ace.Editor>();

  // 計算結果が表示画面が「表示された or 消された」タイミングでエディタのサイズを調整する
  useEffect(
    () => {
      editor?.resize();
    },
    [props.isResultActive]
  );

  return (
    <AceEditor
      // TO-DO: mode="rooplppを作成する"
      theme="tomorrow"
      mode="c_cpp"
      name="rooplpp_program"
      value={props.program}
      editorProps={{ $blockScrolling: true }}
      width="100%"
      height="100%"
      fontSize={14}
      showPrintMargin={true}
      showGutter={true}
      highlightActiveLine={true}
      onLoad={editorInstance => {
        editorInstance.container.style.resize = "both";
        setEditor(editorInstance) // コンポーネントロード時にstateにセット
      }}
      onChange={props.onChange}
    />
  );
}