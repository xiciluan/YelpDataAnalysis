import json
import csv
from collections import defaultdict

user_avgRatings = defaultdict(int)
with open('user.json') as f:
    iter_f = iter(f)
    line = f.readline()
    for line in iter_f:
        d = json.loads(line)
        user_avgRatings[d["user_id"]] = d["average_stars"]
    f.close()

biz_resturants = set()
with open('biz_resturants.csv') as f:
    reader = csv.reader(f)
    for row in reader:
        biz_resturants.add(row[0])



with open('review.json') as f, open('review_summary.csv', 'w') as o:
    iter_f = iter(f)
    writer = csv.writer(o)
    writer.writerow(['date', 'business_id', 'text', 'stars', 'label'])
    line = f.readline()
    for line in iter_f:
        d = json.loads(line)
        if d['business_id'] not in biz_resturants:
            continue
        if d['stars'] >= 4.0:
            label = 'Positive'
        elif d['stars'] <= 2.0:
            label = 'Negative'
        else:
            label = 'Negative' if d['stars'] <= user_avgRatings[d['user_id']] else 'Positive'
        try:
            writer.writerow([d['date'], d['business_id'], d['text'], d['stars'], label])
        except:
            pass


