import csv
import json
from collections import defaultdict

with open('category_biz.csv') as f:
    reader = csv.reader(f)
    fast_food_id = set()
    for row in reader:
        if row[0] == 'Fast Food' or row[0] == 'Pizza' or row[0] == 'Burgers':
            fast_food_id.add(row[1])
    f.close()

# print(fast_food_id)

id_features = defaultdict(lambda: defaultdict(str))
features = ['BusinessParking', 'RestaurantsDelivery', 'RestaurantsReservations', 'OutdoorSeating',
            'NoiseLevel', 'RestaurantsTakeOut', 'RestaurantsPriceRange2', 'WiFi', 'BikeParking', 'RestaurantsGoodForGroups']
info1 = ['name', 'address', 'city', 'state', 'postal_code']
info2 = ['latitude', 'longitude', 'stars', 'review_count']
head = ['bussiness_id'] + info1 + info2 + features

with open('business.json') as f:
    iter_f = iter(f)
    line = f.readline()
    for line in iter_f:
        try:
            d = json.loads(line)
            id = str(d['business_id'])
            attributes = d['attributes']
            if id not in fast_food_id:
                continue
            for i in info1:
                if i in d:
                    id_features[id][i] = str(d[i]).strip().strip('u')
            for i in info2:
                if i in d:
                    id_features[id][i] = d[i]
            for feature in features:
                if attributes and feature in attributes:
                    if feature != 'BusinessParking':
                        id_features[id][feature] = str(attributes[feature]).strip('u').strip('\'')
                    if feature == 'BusinessParking':
                        info = attributes[feature]
                        for c in '\'\".{}()[]:;,.!?':
                            info = info.replace(c, ' ')
                        lst = info.split()
                        id_features[id][feature] = str('True' in lst)
        except:
            pass

    f.close()

with open('biz_fast_food_features.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerow(head)
    for id in id_features.keys():
        writer.writerow([id] + [id_features[id][i] for i in info1] + [id_features[id][i] for i in info2] + [id_features[id][i] for i in features])

