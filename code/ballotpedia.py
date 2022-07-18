import requests, json
import pandas as pd
import bs4

# State abbreviations
states = ['AK','AL','AR','AZ','CA','CO','CT','DC','DE','FL','GA',
          'HI','IA','ID','IL','IN','KS','KY','LA','MA','MD','ME',
          'MI','MN','MO','MS','MT','NC','ND','NE','NH','NJ','NM',
          'NV','NY','OH','OK','OR','PA','RI','SC','SD','TN','TX',
          'UT','VA','VT','WA','WI','WV','WY']

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
    "referer":"https://legislation.ballotpedia.org/"
}

# Build bill list
def build_state_bill_df(st):
    print(st)
    page = 1
    text = requests.post("https://api4.ballotpedia.org/legislation_tracking/search",headers=ballotpedia_headers,json={"state":[st],"page":page}).text
    cur_data = pd.DataFrame(json.loads(text)['data'])
    if cur_data.empty == False:
        total_count = int(cur_data.iloc[1]['total_count'])
        cur_count = cur_data.shape[0]
        while (cur_count < total_count):
            page = page + 1
            text = requests.post("https://api4.ballotpedia.org/legislation_tracking/search",headers=ballotpedia_headers,json={"state":[st],"page":page}).text
            cur_data = pd.concat([cur_data,pd.DataFrame(json.loads(text)['data'])])
            cur_count = cur_data.shape[0]
        return(cur_data.loc[:, cur_data.columns!='total_count'])
    else:
        pass


def main():
    ballotpedia_bill_database = pd.DataFrame(columns = ['id', 'state', 'bill_number', 'name',
                                                        'most_recent_action', 'action_date', 'current_legislative_status',
                                                        'categories'])
    state_dfs = list(map(build_state_bill_df,states))
    ballotpedia_bill_database = pd.concat(state_dfs)

    ballotpedia_bill_database.to_csv("../output/ballotpedia_initial.csv",index=False)


if __name__ == "__main__":
    main()