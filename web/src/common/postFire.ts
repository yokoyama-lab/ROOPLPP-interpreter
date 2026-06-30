import axios, { AxiosResponse } from 'axios';

/**
 * 指定されたエンドポイントに POST し、レスポンスを返す。
 *
 * @param endPoint
 * @param data
 * @returns
 */
export default function postFire(
  endPoint: string,
  data?: { [key in string]: string | number }
): Promise<AxiosResponse> {
  return axios.post(endPoint, data);
}
