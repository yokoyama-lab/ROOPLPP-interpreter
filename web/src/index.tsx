import React from 'react';
import ReactDOM from 'react-dom';
import AceEditor from 'react-ace';
import axios from 'axios';

//import "ace-builds/src-noconflict/mode-java";
import "ace-builds/src-noconflict/theme-github";

interface RooplppWebInterpreterProps {
}
interface RooplppWebInterpreterState {
  program?: string,
  result?: string,
  isInvert?: boolean,
  onClick?: any
}
interface programExample {
  algorithmName: string,
  algorithmSrc: string
}

const HOSTNAME = 'http://localhost:9000/';

const PROGRAME_EXAMPLE_LIST: programExample[] = [
  {algorithmName: 'Fibonacci', algorithmSrc: 'fib.rplpp'},
  {algorithmName: 'Square root', algorithmSrc: 'sqrt.rplpp'},
  {algorithmName: 'Factorization', algorithmSrc: 'factor.rplpp'},
  {algorithmName: 'Perm-to-code', algorithmSrc: 'perm-to-code.rplpp'},
  {algorithmName: 'LinkedList', algorithmSrc: 'LinkedList.rplpp'},
  {algorithmName: 'DoublyLinkedList', algorithmSrc: 'DoublyLinkedList.rplpp'},
  {algorithmName: 'BinaryTree', algorithmSrc: 'BinaryTree.rplpp'},
  {algorithmName: 'BinaryTree_print', algorithmSrc: 'BinaryTree_print.rplpp'},
];

class Editor extends React.Component<RooplppWebInterpreterState> {
  render() {
    return (
      <div>
        <AceEditor
          //mode="java"
          theme="github"
          name="rooplpp_program"
          value={this.props.program}
          editorProps={{$blockScrolling: true}}
        />
      </div>
    );
  }
}

class ProgramList extends React.Component<RooplppWebInterpreterState> {
  render() {
    const listItems = PROGRAME_EXAMPLE_LIST.map((programExample) =>
      <li onClick={() => this.props.onClick(programExample.algorithmSrc)}>{programExample.algorithmName}</li>
    );
    return (
      <ul>{listItems}</ul>
    );
  }
}

class ExecuteResult extends React.Component<RooplppWebInterpreterState> {
  render() {
    return (
      <div className='result'>{this.props.result}</div>
    );
  }
}

class RooplppWebInterpreter extends React.Component<RooplppWebInterpreterProps, RooplppWebInterpreterState> {
  constructor(props: RooplppWebInterpreterProps) {
    super(props);
    this.state = {
      program: '',
      result: '',
      isInvert: false
    }
  }

  // executeボタンがクリックされた時の処理
  // resultに結果を表示する
  handleExecuteClick() {
    // 非同期処理
    axios({
      method: 'POST',
      url: HOSTNAME + '/execute.php',
      data: {
        prog: this.state.program,
        invert: this.state.isInvert ? 1 : 0
      }
    }).then(response => {
      console.log(response.status);
      console.log(response.data[0])
      this.setState({
        result: response.data[0]
      });
    }).catch(err => {
      console.log('err: ', err);
      this.setState({
        result: err
      });
    });
  }

  // プログラムサンプルリストのアイテムがクリックされた時の処理
  // editor内のプログラムを更新する
  handleExampleClick(algorithmSrc: string) {
    // 非同期処理
    axios({
      method: 'GET',
      url: HOSTNAME + '/example.php',
      params: {
        filename: algorithmSrc
      }
    }).then(response => {
      console.log(response.status);
      this.setState({
        program: response.data[0]
      });
    }).catch(err => {
      console.log('err: ', err);
      this.setState({
        program: err
      });
    })
  }

  handleInvertClick() {
    this.setState({
      isInvert: !this.state.isInvert
    });
  }

  handleOnClick(){
    this.setState({
      program: 'succsess',
    });
  }

  render() {
    return (
      <div>
        <Editor program={this.state.program} />
        <ProgramList onClick={(algorithmSrc: string) => this.handleExampleClick(algorithmSrc)} />
        <ExecuteResult result={this.state.result} />
        <button onClick={this.handleExecuteClick.bind(this)}>Execute</button>
        <button onClick={this.handleInvertClick.bind(this)}>invert</button>
      </div>
    );
  }
}

window.onload = () => {
  ReactDOM.render(<RooplppWebInterpreter />, document.getElementById('root'));
};