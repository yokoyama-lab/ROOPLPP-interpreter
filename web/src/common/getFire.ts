import axios, { AxiosResponse } from 'axios';

/**
 * 指定されたエンドポイントにアクセスし、
 * 取得した結果を返す(getメソッド用)
 * 
 * @param endPoint 
 * @param params 
 * @returns 
 */
export default function getFire(
  endPoint: string,
  params?: { [key in string]: string | number }
): Promise<AxiosResponse> {
  return new Promise((resolve, reject) => {
    axios({
      method: 'GET',
      url: endPoint,
      params: params
    }).then(response => {
      resolve(response);
    }).catch(err => {
      reject(err)
    })
  })
}