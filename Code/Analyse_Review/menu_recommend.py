import csv
import json
from collections import defaultdict
rating_cnt = defaultdict(int)
pos_neg_cnt = defaultdict(int)

pos = {'good', 'great', 'excellent', 'fantastic', 'perfect'}
neg = {'bad', 'awful', 'terrible', 'disgusting', 'shit'}
with open('reviews_clean.csv') as f:
    reader = csv.reader(f)
    for row in reader:
        text, star = row[1].split(), row[2]
        i, n = 0, len(text)
        while i < n and text[i] != 'veggie':
            i += 1
        if i == n:
            continue
        rating_cnt[star] += 1
        left, right = max(0, i-10), min(n, i+10)
        for word in text[left:right]:
            if word in pos:
                pos_neg_cnt['pos'] += 1
            if word in neg:
                pos_neg_cnt['neg'] += 1

    f.close()

print(rating_cnt)
print(pos_neg_cnt)
