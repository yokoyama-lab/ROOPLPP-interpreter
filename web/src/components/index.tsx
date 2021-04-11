import React from 'react';
import ReactDOM from 'react-dom';
import axios from 'axios';
import className from 'classnames';

import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import Button from '@material-ui/core/Button';
import PlayCircleOutlineIcon from '@material-ui/icons/PlayCircleOutline';
import IconButton from '@material-ui/core/IconButton';
import CloseIcon from '@material-ui/icons/Close';
import CssBaseline from '@material-ui/core/CssBaseline';

import ProgramList from './ProgramList';
import Option from './Option';
import Editor from './Editor';

import '../scss/index.scss';

// ドメイン名を取得
// 非同期通信での送信先URLの生成に使用
const HOSTNAME = document.URL;

type OptionIndex = 'isInvert' | 'isImportLibrary';
interface RooplppWebInterpreterProps {
}
interface RooplppWebInterpreterState {
  program: string
  result: {
    value: string
    isActive: boolean
  }
  option: {
    [key in OptionIndex]: boolean
  }
  example?: string // 例の表示をselectで行う間の仮のもの
}

class RooplppWebInterpreter extends React.Component<RooplppWebInterpreterProps, RooplppWebInterpreterState> {
  constructor(props: RooplppWebInterpreterProps) {
    super(props);
    this.state = {
      program: '',
      result: {
        value: '',
        isActive: false
      },
      option: {
        isInvert: false,
        isImportLibrary: false
      }
    }
  }

  // executeボタンがクリックされた時の処理
  // resultに結果を表示する
  handleExecuteClick() {
    // 関数動作条件
    if (this.state.option?.isInvert == undefined && this.state.option?.isImportLibrary == undefined) {
      return;
    }

    axios({
      method: 'POST',
      url: HOSTNAME + '/execute.php',
      data: {
        prog: this.state.program,
        invert: this.state.option.isInvert ? 1 : 0,
        library: this.state.option.isImportLibrary ? 1 : 0,
      }
    }).then(response => {
      const state = Object.assign({}, this.state, { result: { value: response.data[0], isActive: true } });
      this.setState(state);
    }).catch(err => {
      console.error('err: ', err);
      const state = Object.assign({}, this.state, { result: { value: err, isActive: true } });
      this.setState(state);
    });
  }

  // プログラムサンプルリストのアイテムがクリックされた時の処理
  // editor内のプログラムを更新する
  handleExampleClick(algorithmSrc: string) {
    axios({
      method: 'GET',
      url: HOSTNAME + '/example.php',
      params: {
        filename: algorithmSrc
      }
    }).then(response => {
      console.log(response);
      const state = Object.assign({}, this.state, { program: response.data[0] });
      this.setState(state);
    }).catch(err => {
      console.log('err: ', err);
      const state = Object.assign({}, this.state, { program: err });
      this.setState(state);
    })
  }

  // プログラムサンプルリストのアイテムがチェンジされた時の処理
  // 例の表示をselectで行う間の仮のもの
  // editor内のプログラムを更新する
  handleExampleChange(event: React.ChangeEvent<HTMLSelectElement>) {
    const target = event.target;
    const value = target.value;
    const name = target.name;

    const state = Object.assign({}, this.state, { example: value });
    this.setState(state);

    if (value != '') {
      axios({
        method: 'GET',
        url: HOSTNAME + '/example.php',
        params: {
          filename: value
        }
      }).then(response => {
        console.log(response);
        const state = Object.assign({}, this.state, { program: response.data[0] });
        this.setState(state);
      }).catch(err => {
        console.log('err: ', err);
        const state = Object.assign({}, this.state, { program: err });
        this.setState(state);
      })
    } else {
      const state = Object.assign({}, this.state, { program: '' });
      this.setState(state);
    }
  }

  // editor内の文字列が変化した時の処理
  handleEditorChange(newValue: string) {
    this.setState({
      program: newValue
    });
  }

  // resultを隠すボタンがクリックされた時の処理
  handleResultHideClick() {
    // resultのisActiveを反転させる
    const result = Object.assign({}, this.state.result, { isActive: !this.state.result?.isActive });
    const state = Object.assign({}, this.state, { result: result });

    this.setState(state);
  }

  // オプションがクリックされたときの処理 (invert, import library)
  handleOptionChange(event: React.ChangeEvent<HTMLInputElement>) {
    const target = event.target;
    const value = target.type == 'checkbox' ? target.checked : target.value;
    const name = target.name;

    // 現在のoptionのnameをvalueに更新してコピー
    const option = Object.assign({}, this.state.option, { [name]: value });
    const state = Object.assign({}, this.state, { option: option });

    this.setState(state);
  }

  render() {
    return (
      <>
        {// CSSリセット
        }
        <CssBaseline />
        
        <AppBar position="sticky" color="inherit">
          <Toolbar>
            <Typography variant="h6">Roopl++ Online Interpreter</Typography>

            {// 実行ボタン
            }
            <Button
              variant="outlined"
              size="small"
              startIcon={<PlayCircleOutlineIcon />}
              onClick={this.handleExecuteClick.bind(this)}
              className="margin-left-xxxl--important"
            >
              Execute
            </Button>
            { // プログラム例のリスト
            }
            <div className="margin-left-xxxl--important">
              <ProgramList
                onExampleChange={(event: React.ChangeEvent<HTMLSelectElement>) => this.handleExampleChange(event)}
                onClick={(algorithmSrc: string) => this.handleExampleClick(algorithmSrc)}
              />
            </div>

            { // オプション選択部分
            }
            <div className="margin-left-4xl--important">
              <Option
                option={this.state.option}
                onChange={this.handleOptionChange.bind(this)}
              />
            </div>

          </Toolbar>
        </AppBar>

        { // エディタ部分 
        }
        <Editor 
          isResultActive={this.state.result.isActive}
          program={this.state.program}
          onChange={(newValue: string) => this.handleEditorChange(newValue)}
        />

        { // 計算結果表示部分
        }
        <div id="result" className={
          className(
            "background-color-grey",
            {
              "active": this.state.result.isActive,
            })
        }>
          {// 閉じるボタン
          }
          <IconButton
            aria-label="close"
            size="small"
            edge={false}
            onClick={() => this.handleResultHideClick()}
          >
            <CloseIcon />
          </IconButton>

          {// 結果
          }
          <textarea
            disabled
            readOnly={true}
            value={this.state.result.value}
            className="background-color-white"
          />
        </div>

      </>
    );
  }
}

window.onload = () => {
  ReactDOM.render(<RooplppWebInterpreter />, document.getElementById('root'));
};