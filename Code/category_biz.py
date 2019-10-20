import json
import csv
from collections import defaultdict
with open('business.json') as f:
    iter_f = iter(f)
    line = f.readline()
    biz_list = []
    for line in iter_f:
        d = json.loads(line)
        biz_list.append(d)
    f.close()

# category = ['Beauty & Spas', 'Restaurants',
#             'Shopping', 'Hotels & Travel', 'Home Services', 'Automotive', 'Event Planning & Services', 'other']


category_summary = defaultdict(list)
for biz in biz_list:
    id = biz['business_id']
    ctg = biz['categories']
    if not ctg:
        continue
    ctg = ctg.split(',')
    for i in range(len(ctg)):
        ctg[i] = ctg[i].strip()
    for ele in ctg:
        category_summary[ele].append(id)

# print(category_summary)

with open('category_biz.csv', 'w') as f:
    writer = csv.writer(f)
    # writer.writerow(['Category', 'Business_id'])
    for ctg in category_summary:
        for biz in category_summary[ctg]:
            writer.writerow([ctg, biz])
