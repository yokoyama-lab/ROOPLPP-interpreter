import React from 'react';
import FormControlLabel from '@mui/material/FormControlLabel';
import Checkbox from '@mui/material/Checkbox';

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
      <FormControlLabel
        control={
          <Checkbox
            name="isInvert"
            checked={props.option != undefined && props.option.isInvert}
            color="default"
            inputProps={{ 'aria-label': 'checkbox with default color' }}
            onChange={props.onChange}
          />
        }
        label="Invert"
        labelPlacement="end"
      />
      <FormControlLabel
        control={
          <Checkbox
            name="isImportLibrary"
            checked={props.option != undefined && props.option.isImportLibrary}
            color="default"
            inputProps={{ 'aria-label': 'checkbox with default color' }}
            onChange={props.onChange}
          />
        }
        label="Import Library"
        labelPlacement="end"
      />
    </div>
  );
}