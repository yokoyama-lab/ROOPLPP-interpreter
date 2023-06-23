import React, { useCallback, useEffect, useState } from 'react';
import ReactDOM from 'react-dom';
import className from 'classnames';

import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import Button from '@material-ui/core/Button';
import PlayCircleOutlineIcon from '@material-ui/icons/PlayCircleOutline';
import IconButton from '@material-ui/core/IconButton';
import CloseIcon from '@material-ui/icons/Close';
import ShareIcon from '@material-ui/icons/Share';
import CssBaseline from '@material-ui/core/CssBaseline';
import Popover from '@material-ui/core/Popover';
import TextField from '@material-ui/core/TextField';

import getFire from '../common/getFire';
import postFire from '../common/postFire';

import ProgramList from './ProgramList';
import Option from './Option';
import Editor from './Editor';

import '../scss/index.scss';

// URLを取得(パラメータを除く)
// 非同期通信での送信先URLの生成に使用
const HOSTNAME = document.URL.split('?')[0];

function App() {
  const [program, setProgram] = useState(''); // エディタに記述したrooplppプログラム
  const [result, setResult] = useState(''); // rooplppプログラムの計算結果
  const [isResultOpen, setIsResultOpen] = useState(false) // 計算結果表示部分を表示しているかのフラグ
  const [isInvert, setIsInvert] = useState(false) // オプション Invert
  const [isImportLibrary, setIsImportLibrary] = useState(false) // オプション isImportLibrary

  const [shareUrl, setShareUrl] = useState(''); // シェア用のURL
  const [anchorEl, setAnchorEl] = React.useState<HTMLButtonElement | null>(null); // シェアボタン用

  /**
   * サーバに保存されたプログラムをロードする
   */
  const getSavedProgram = async (hash: string) => {
    try {
      const response = await getFire(HOSTNAME + '/load.php', { hash: hash });
      setProgram(response.data[0]);
    } catch (err) {
      // setProgram(err);
      console.log(err);
    }
  }

  useEffect(
    () => {
      const url = new URL(window.location.href);
      const params = url.searchParams;
      if (params.get('hash') != null) {
        getSavedProgram(params.get('hash') as string);
      }
    },
    []
  );

  /**
   * executeボタンがクリックされた時、プログラムの計算結果を取得し、resultに表示する
   */
  const handleExecuteClick = useCallback(
    async () => {
      try {
        const response = await postFire(HOSTNAME + '/execute.php', {
          prog: program,
          invert: isInvert ? 1 : 0,
          library: isImportLibrary ? 1 : 0,
        });
        setResult(response.data[0]);
      } catch (err) {
        //setResult(err);
        console.log(err);
      }
      setIsResultOpen(true);
    },
    [program, isInvert, isImportLibrary]
  );

  /**
   * プログラムサンプルリストのアイテムがクリックされた時、サンプルを取得し、エディタの内容を更新する
   */
  const handleExampleClick = useCallback(
    async (algorithmSrc: string) => {
      try {
        const response = await getFire(HOSTNAME + '/example.php', { filename: algorithmSrc });
        setProgram(response.data[0]);
      } catch (err) {
        //setProgram(err);
        console.log(err);
      }
    },
    []
  );

  /**
   * シェアボタンがクリックされた時の処理
   */
  const handleShareClick = useCallback(
    async (event: React.MouseEvent<HTMLButtonElement>) => {
      setAnchorEl(event.currentTarget);
      try {
        const response = await postFire(HOSTNAME + '/share.php', { prog: program });
        setShareUrl(`${HOSTNAME}?hash=${response.data[0]}`);
      } catch (err) {
        // To-Do: エラー処理を実装する
      }
    },
    [program]
  );

  /**
   * シェアボタンをクリックしたときに表示されるポップアップを削除する処理
   */
  const handleShareURLClose = useCallback(
    () => {
      setAnchorEl(null);
      setShareUrl(``);
    },
    []
  );

  /**
   * オプションがクリックされたときの処理 (invert, import library)
   */
  const handleOptionClick = useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      const target = event.target;
      const value = target.type == 'checkbox' ? target.checked : target.value;
      const name = target.name;

      if (name == 'isInvert') {
        setIsInvert(value as boolean);
      }
      if (name == 'isImportLibrary') {
        setIsImportLibrary(value as boolean);
      }
    },
    []
  );

  /**
   * エディタの内容が変更した時の処理
   */
  const handleProgramChange = useCallback(
    (program: string) => {
      setProgram(program);
    },
    []
  );

  /**
   * 計算結果表示部分の閉じるボタンが押された時
   */
  const handleResultCloseButtonClick = useCallback(
    () => {
      setIsResultOpen(false);
    },
    []
  );

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
            onClick={handleExecuteClick}
            className="margin-left-xxxl--important"
          >
            Execute
          </Button>
          { // プログラム例のリスト
          }
          <div className="margin-left-xxxl--important">
            <ProgramList
              onClick={(algorithmSrc: string) => handleExampleClick(algorithmSrc)}
            />
          </div>

          {// シェアボタン
          }
          <Button
            variant="text"
            size="small"
            startIcon={<ShareIcon />}
            onClick={handleShareClick}
            className="margin-left-xxxl--important"
          >
            Share
          </Button>
          <Popover
            open={anchorEl != null ? true : false}
            anchorEl={anchorEl}
            onClose={handleShareURLClose}
            anchorOrigin={{
              vertical: 'bottom',
              horizontal: 'center',
            }}
            transformOrigin={{
              vertical: 'top',
              horizontal: 'center',
            }}
          >
            <div
              className={className(
                'padding-top-md',
                'padding-bottom-md',
                'padding-left-md',
                'padding-right-md'
              )}
            >
              <TextField
                size='small'
                value={shareUrl}
                label="URL"
                variant="outlined"
                InputProps={{
                  readOnly: true,
                }}
                style = {{width: '300px'}}
              />
            </div>
          </Popover>

          { // オプション選択部分
          }
          <div className="margin-left-4xl--important">
            <Option
              option={
                {
                  isInvert: isInvert,
                  isImportLibrary: isImportLibrary
                }
              }
              onChange={handleOptionClick}
            />
          </div>
        </Toolbar>
      </AppBar>

      { // エディタ部分(計算結果が存在するとき大きさが縮小する)
      }
      <div id="ace-editor" className={className({
        "is-result-open": isResultOpen
      })}
      >
        <Editor
          isResultActive={isResultOpen}
          program={program}
          onChange={(newValue: string) => handleProgramChange(newValue)}
        />
      </div>

      { // 計算結果表示部分(計算結果が存在するときに表示される)
      }
      <div id="result" className={
        className(
          "background-color-grey",
          {
            "active": isResultOpen
          })
      }>
        {// 閉じるボタン
        }
        <IconButton
          aria-label="close"
          size="small"
          edge={false}
          onClick={handleResultCloseButtonClick}
        >
          <CloseIcon />
        </IconButton>

        {// 結果
        }
        <textarea
          disabled
          readOnly={true}
          value={result}
          className="background-color-white"
        />
      </div>
    </>
  );
}

window.onload = () => {
  ReactDOM.render(<App />, document.getElementById('root'));
};