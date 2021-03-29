import React from 'react';

interface OptionProps {
  onChange: (event: React.ChangeEvent<HTMLInputElement>) => void
  option?: {
    isInvert?: boolean
    isImportLibrary?: boolean
  }
}

// オプション部分
export default function Option(props: OptionProps) {
  return (
    <div className="option">
      <input
        type="checkbox"
        name="isInvert"
        checked={props.option != undefined && props.option.isInvert}
        onChange={props.onChange} />
      Invert
      <input
        type="checkbox"
        name="isImportLibrary"
        checked={props.option != undefined && props.option.isImportLibrary}
        onChange={props.onChange} />
      Import Library
    </div>
  );
}