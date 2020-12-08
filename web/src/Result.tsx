import React from 'react';

interface ResultProps {
  result: {
    value: string
    isActive: boolean
  }
  onResultHideBtnClick: () => void
}

// 計算結果表示部分
export default function Result(props: ResultProps) {
  return (
    <div id="result" className={props.result.isActive == true ? "active" : "deactive"}>
      <button onClick={props.onResultHideBtnClick}>✖️</button>
      <textarea
        value={props.result.value}
        readOnly={true} />
    </div>
  );
}