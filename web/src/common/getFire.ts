import axios, { AxiosResponse } from 'axios';

/**
 * 指定されたエンドポイントに GET し、レスポンスを返す。
 *
 * @param endPoint
 * @param params
 * @returns
 */
export default function getFire(
  endPoint: string,
  params?: { [key in string]: string | number }
): Promise<AxiosResponse> {
  return axios.get(endPoint, { params });
}
