import React from 'react';
import ReactDOM from 'react-dom';
import AceEditor from 'react-ace';
import axios from 'axios';

import ProgramList from './ProgramList';
import Option from './Option';
import Result from './Result';

// TO-DO: mode="rooplppを作成する"
// ace-editorのシンタックスハイライト
// import "ace-builds/src-noconflict/mode-java";

// ace-editorのテーマ
import "ace-builds/src-noconflict/theme-github";

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
      console.log(response);
      const state = Object.assign({}, this.state, { result: { value: response.data[0], isActive: true } });
      this.setState(state);
    }).catch(err => {
      console.log('err: ', err);
      const state = Object.assign({}, this.state, { result: { value: err, isActive: true } });
      this.setState(state);
    });

    console.log(this.state);

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
      <div id='rooplpp_web_interpreter'>
        <header>
          <h3 id='header_title'>Roopl++ Online Interpreter</h3>
          <button id='execute_btn' onClick={this.handleExecuteClick.bind(this)}>Execute</button>

          { // プログラム例のリスト
          }
          <ProgramList onClick={(algorithmSrc: string) => this.handleExampleClick(algorithmSrc)} />

          { // オプション選択部分
          }
          <Option
            option={this.state.option}
            onChange={this.handleOptionChange.bind(this)}
          />
        </header>

        { // エディタ部分 
        }
        <body>
          <AceEditor
            // TO-DO: mode="rooplppを作成する"
            // mode="java"
            theme="github"
            name="rooplpp_program"
            value={this.state.program}
            editorProps={{ $blockScrolling: true }}
            onChange={(newValue: string) => this.handleEditorChange(newValue)}
            width={`${window.innerWidth}px`}
            height={`${window.innerHeight - 50}px`}
          />


        </body>
        { // 計算結果表示部分
        }
        <Result
          result={this.state.result}
          onResultHideBtnClick={() => this.handleResultHideClick()}
        />

      </div>
    );
  }
}

window.onload = () => {
  ReactDOM.render(<RooplppWebInterpreter />, document.getElementById('root'));
};