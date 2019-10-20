import csv
from collections import defaultdict

ctg_cnt = defaultdict(int)
with open('category_biz.csv') as f:
    reader = csv.reader(f)
    for row in reader:
        ctg_cnt[row[0]] += 1

with open('category_count.csv', 'w') as f:
    writer = csv.writer(f)
    for ctg in ctg_cnt:
        writer.writerow([ctg, ctg_cnt[ctg]])

