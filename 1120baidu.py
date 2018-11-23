import requests
from bs4 import BeautifulSoup


headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36'}
base_url ='https://www.baidu.com/s?wd=%E8%85%BE%E8%AE%AF%E6%96%B0%E9%97%BB&pn=0&oq=%E8%85%BE%E8%AE%AF%E6%96%B0%E9%97%BB&tn=baiduhome_pg&ie=utf-8&usm=1&rsv_idx=2&rsv_pq=9be78fbc0001122b&rsv_t=bd32EZJUSU75EwOCspZ9PES9M0ts6x41fh5bJ6PltgxIf91InxjTcwsKznWUCVf0d%2BPR'



for num in range(1,5):
    print('第{}页',format(num*10))
    r = requests.get('https://www.baidu.com/s?wd=%E8%85%BE%E8%AE%AF%E6%96%B0%E9%97%BB&pn='+str(num*10)+'&oq=%E8%85%BE%E8%AE%AF%E6%96%B0%E9%97%BB&tn=baiduhome_pg&ie=utf-8&usm=1&rsv_idx=2&rsv_pq=9be78fbc0001122b&rsv_t=bd32EZJUSU75EwOCspZ9PES9M0ts6x41fh5bJ6PltgxIf91InxjTcwsKznWUCVf0d%2BPR', headers = headers)
    content = r.text
    soup = BeautifulSoup(r.text, 'lxml')

    divs = soup.find_all(class_='c-abstract')

    for div in divs:
        abstract = div.get_text()
        print(abstract)
        print('------')



