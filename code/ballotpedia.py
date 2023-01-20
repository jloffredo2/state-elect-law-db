import requests, json
import pandas as pd
import logging

# State abbreviations
states = ['AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL', 'GA',
          'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 'MD', 'ME',
          'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 'NJ', 'NM',
          'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX',
          'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 'WY']

# HTML request
ballotpedia_headers = {
    "sec-ch-ua": """.Not/A)Brand";v="99", "Google Chrome";v="103", "Chromium";v="103""",
    "accept-language": "en-US,en;q=0.9",
    "sec-ch-ua-mobile": """?0""",
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36",
    "Content-Type": "application/json",
    "accept": "application/json",
    "sec-ch-ua-platform": "macOS",
    "Origin": "https://legislation.ballotpedia.org",
    "Sec-Fetch-Site": "same-site",
    "Sec-Fetch-Mode": "cors",
    "Sec-Fetch-Dest": "empty",
    "Accept-Encoding": "gzip, deflate, br",
    "referer": "https://legislation.ballotpedia.org/"
}

billtrack_headers = {
    "sec-ch-ua": """.Not/A)Brand";v="99", "Google Chrome";v="103", "Chromium";v="103""",
    "accept-language": "en-US,en;q=0.9",
    "sec-ch-ua-mobile": """?0""",
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36",
    "accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
    "cache-control": "max-age=0",
    "upgrade-insecure-requests": "1",
    "sec-ch-ua-platform": "macOS",
    "Sec-Fetch-Site": "none",
    "Sec-Fetch-Mode": "navigate",
    "Sec-Fetch-Dest": "document",
    "Accept-Encoding": "gzip, deflate, br",
    "sec-fetch-user": "?1"
}


# Build bill list
def build_state_bill_df(st):
    logging.info("Retrieving state data in bulk: {}".format(st))
    page = 1
    text = requests.post("https://api4.ballotpedia.org/legislation_tracking/search", headers=ballotpedia_headers,
                         json={"state": [st], "page": page, "topic_area":1, "session": ["2022", "2023"]}).text
    cur_data = pd.DataFrame(json.loads(text)['data'])
    if cur_data.empty == False:
        total_count = int(cur_data.iloc[1]['total_count'])
        cur_count = cur_data.shape[0]
        while (cur_count < total_count):
            page = page + 1
            text = requests.post("https://api4.ballotpedia.org/legislation_tracking/search",
                                 headers=ballotpedia_headers, json={"state": [st], "page": page, "topic_area":1}).text
            cur_data = pd.concat([cur_data, pd.DataFrame(json.loads(text)['data'])])
            cur_count = cur_data.shape[0]
        return (cur_data.loc[:, cur_data.columns != 'total_count'])
    else:
        pass


def extract_bill_info(id):
    logging.info("Bill info (ID: {})".format(id))
    return (json.loads(
        requests.post("https://api4.ballotpedia.org/legislation_tracking/bill", headers=ballotpedia_headers,
                      json={"bill": id}).text)['data'][0])


def main():
    logging.basicConfig(level = logging.INFO,
                        format = '%(asctime)s:%(levelname)s:%(message)s')

    ballotpedia_df = pd.DataFrame(columns=['id', 'state', 'bill_number', 'name',
                                           'most_recent_action', 'action_date', 'current_legislative_status',
                                           'categories'])
    state_dfs = list(map(build_state_bill_df, states))
    ballotpedia_df = pd.concat(state_dfs)

    bill_metadata = list(map(extract_bill_info, ballotpedia_df.id))
    bill_metadata = pd.DataFrame(bill_metadata )[
        ['id', 'bill_track_link', 'session_year', 'summary', 'number_of_sponsors', 'trifecta_status',
         'sponsors_partisan_affiliations']]

    ballotpedia_df = ballotpedia_df.merge(bill_metadata, how='left', on='id')
    ballotpedia_df.to_csv("output/ballotpedia_initial.csv", index=False)


if __name__ == "__main__":
    main()
