import axios, { AxiosResponse } from 'axios';

/**
 * 指定されたエンドポイントにアクセスし、
 * 取得した結果を返す(postメソッド用)
 * 
 * @param endPoint 
 * @param data 
 * @returns 
 */
export default function getFire(
  endPoint: string,
  data?: { [key in string]: string | number }
): Promise<AxiosResponse> {
  return new Promise((resolve, reject) => {
    axios({
      method: 'POST',
      url: endPoint,
      data: data
    }).then(response => {
      resolve(response);
    }).catch(err => {
      reject(err);
    });
  })
}