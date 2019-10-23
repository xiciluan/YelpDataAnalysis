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
features = ["BusinessParking", "RestaurantsDelivery", "RestaurantsReservations", 
            "NoiseLevel", "RestaurantsTakeOut", "RestaurantsPriceRange2", "WiFi"]
head = ['business_id', 'stars', 'latitude', 'longitude'] + features
with open('business.json') as f:
    iter_f = iter(f)
    line = f.readline()
    for line in iter_f:
        d = json.loads(line)
        id = d['business_id']
        attributes = d['attributes']
        if id not in fast_food_id:
            continue
        id_features[id]['stars'] = d['stars']
        id_features[id]['latitude'] = d['latitude']
        id_features[id]['longitude'] = d['longitude']
        for feature in features:
            if attributes and feature in attributes:
                if feature != "BusinessParking":
                    id_features[id][feature] = attributes[feature].strip('u').strip('\'')
                else:
                    info = attributes[feature]
                    for c in '\'\".{}()[]:;,.!?':
                        info = info.replace(c, ' ')
                    lst = info.split()
                    id_features[id][feature] = str('True' in lst)

    f.close()
# print(id_features)


with open('biz_fast_food.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerow(head)
    for id in id_features:
        writer.writerow([id, id_features[id]['stars'], id_features[id]['latitude'], id_features[id]['longitude']]+[id_features[id][feature] for feature in features])

