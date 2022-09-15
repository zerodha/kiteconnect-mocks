#import "orders.h"

#define λ(decl, expr) (^(decl) { return (expr); })

static id NSNullify(id _Nullable x) {
    return (x == nil || x == NSNull.null) ? NSNull.null : x;
}

NS_ASSUME_NONNULL_BEGIN

@interface QTOrders (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTDefinitions (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTDatum (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTDatumProperties (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTAveragePrice (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTExchangeOrderID (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTExchangeETimestamp (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTOrderTimestamp (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTMeta (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTTags (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTIceberg (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTIcebergProperties (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTMetaClass (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTMetaProperties (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTOrdersClass (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTOrdersProperties (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTData (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@implementation QTType
+ (NSDictionary<NSString *, QTType *> *)values
{
    static NSDictionary<NSString *, QTType *> *values;
    return values = values ? values : @{
        @"boolean": [[QTType alloc] initWithValue:@"boolean"],
        @"integer": [[QTType alloc] initWithValue:@"integer"],
        @"null": [[QTType alloc] initWithValue:@"null"],
        @"string": [[QTType alloc] initWithValue:@"string"],
    };
}

+ (QTType *)boolean { return QTType.values[@"boolean"]; }
+ (QTType *)integer { return QTType.values[@"integer"]; }
+ (QTType *)null { return QTType.values[@"null"]; }
+ (QTType *)string { return QTType.values[@"string"]; }

+ (instancetype _Nullable)withValue:(NSString *)value
{
    return QTType.values[value];
}

- (instancetype)initWithValue:(NSString *)value
{
    if (self = [super init]) _value = value;
    return self;
}

- (NSUInteger)hash { return _value.hash; }
@end

static id map(id collection, id (^f)(id value)) {
    id result = nil;
    if ([collection isKindOfClass:NSArray.class]) {
        result = [NSMutableArray arrayWithCapacity:[collection count]];
        for (id x in collection) [result addObject:f(x)];
    } else if ([collection isKindOfClass:NSDictionary.class]) {
        result = [NSMutableDictionary dictionaryWithCapacity:[collection count]];
        for (id key in collection) [result setObject:f([collection objectForKey:key]) forKey:key];
    }
    return result;
}

#pragma mark - JSON serialization

QTOrders *_Nullable QTOrdersFromData(NSData *data, NSError **error)
{
    @try {
        id json = [NSJSONSerialization JSONObjectWithData:data options:NSJSONReadingAllowFragments error:error];
        return *error ? nil : [QTOrders fromJSONDictionary:json];
    } @catch (NSException *exception) {
        *error = [NSError errorWithDomain:@"JSONSerialization" code:-1 userInfo:@{ @"exception": exception }];
        return nil;
    }
}

QTOrders *_Nullable QTOrdersFromJSON(NSString *json, NSStringEncoding encoding, NSError **error)
{
    return QTOrdersFromData([json dataUsingEncoding:encoding], error);
}

NSData *_Nullable QTOrdersToData(QTOrders *orders, NSError **error)
{
    @try {
        id json = [orders JSONDictionary];
        NSData *data = [NSJSONSerialization dataWithJSONObject:json options:kNilOptions error:error];
        return *error ? nil : data;
    } @catch (NSException *exception) {
        *error = [NSError errorWithDomain:@"JSONSerialization" code:-1 userInfo:@{ @"exception": exception }];
        return nil;
    }
}

NSString *_Nullable QTOrdersToJSON(QTOrders *orders, NSStringEncoding encoding, NSError **error)
{
    NSData *data = QTOrdersToData(orders, error);
    return data ? [[NSString alloc] initWithData:data encoding:encoding] : nil;
}

@implementation QTOrders
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"$ref": @"ref",
        @"$schema": @"schema",
        @"definitions": @"definitions",
    };
}

+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error
{
    return QTOrdersFromData(data, error);
}

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error
{
    return QTOrdersFromJSON(json, encoding, error);
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTOrders alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _definitions = [QTDefinitions fromJSONDictionary:(id)_definitions];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTOrders.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTOrders.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTOrders.properties.allValues] mutableCopy];

    for (id jsonName in QTOrders.properties) {
        id propertyName = QTOrders.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    [dict addEntriesFromDictionary:@{
        @"definitions": [_definitions JSONDictionary],
    }];

    return dict;
}

- (NSData *_Nullable)toData:(NSError *_Nullable *)error
{
    return QTOrdersToData(self, error);
}

- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error
{
    return QTOrdersToJSON(self, encoding, error);
}
@end

@implementation QTDefinitions
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"Datum": @"datum",
        @"Iceberg": @"iceberg",
        @"Meta": @"meta",
        @"Orders": @"orders",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTDefinitions alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _datum = [QTDatum fromJSONDictionary:(id)_datum];
        _iceberg = [QTIceberg fromJSONDictionary:(id)_iceberg];
        _meta = [QTMetaClass fromJSONDictionary:(id)_meta];
        _orders = [QTOrdersClass fromJSONDictionary:(id)_orders];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTDefinitions.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTDefinitions.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTDefinitions.properties.allValues] mutableCopy];

    for (id jsonName in QTDefinitions.properties) {
        id propertyName = QTDefinitions.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    [dict addEntriesFromDictionary:@{
        @"Datum": [_datum JSONDictionary],
        @"Iceberg": [_iceberg JSONDictionary],
        @"Meta": [_meta JSONDictionary],
        @"Orders": [_orders JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTDatum
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"additionalProperties": @"isAdditionalProperties",
        @"properties": @"properties",
        @"required": @"required",
        @"title": @"title",
        @"type": @"type",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTDatum alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _properties = [QTDatumProperties fromJSONDictionary:(id)_properties];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTDatum.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTDatum.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTDatum.properties.allValues] mutableCopy];

    for (id jsonName in QTDatum.properties) {
        id propertyName = QTDatum.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    [dict addEntriesFromDictionary:@{
        @"additionalProperties": _isAdditionalProperties ? @YES : @NO,
        @"properties": [_properties JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTDatumProperties
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"average_price": @"averagePrice",
        @"cancelled_quantity": @"cancelledQuantity",
        @"disclosed_quantity": @"disclosedQuantity",
        @"exchange": @"exchange",
        @"exchange_order_id": @"exchangeOrderID",
        @"exchange_timestamp": @"exchangeTimestamp",
        @"exchange_update_timestamp": @"exchangeUpdateTimestamp",
        @"filled_quantity": @"filledQuantity",
        @"guid": @"guid",
        @"instrument_token": @"instrumentToken",
        @"market_protection": @"marketProtection",
        @"meta": @"meta",
        @"modified": @"modified",
        @"order_id": @"orderID",
        @"order_timestamp": @"orderTimestamp",
        @"order_type": @"orderType",
        @"parent_order_id": @"parentOrderID",
        @"pending_quantity": @"pendingQuantity",
        @"placed_by": @"placedBy",
        @"price": @"price",
        @"product": @"product",
        @"quantity": @"quantity",
        @"status": @"status",
        @"status_message": @"statusMessage",
        @"status_message_raw": @"statusMessageRaw",
        @"tag": @"tag",
        @"tags": @"tags",
        @"tradingsymbol": @"tradingsymbol",
        @"transaction_type": @"transactionType",
        @"trigger_price": @"triggerPrice",
        @"validity": @"validity",
        @"validity_ttl": @"validityTTL",
        @"variety": @"variety",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTDatumProperties alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _averagePrice = [QTAveragePrice fromJSONDictionary:(id)_averagePrice];
        _cancelledQuantity = [QTAveragePrice fromJSONDictionary:(id)_cancelledQuantity];
        _disclosedQuantity = [QTAveragePrice fromJSONDictionary:(id)_disclosedQuantity];
        _exchange = [QTAveragePrice fromJSONDictionary:(id)_exchange];
        _exchangeOrderID = [QTExchangeOrderID fromJSONDictionary:(id)_exchangeOrderID];
        _exchangeTimestamp = [QTExchangeETimestamp fromJSONDictionary:(id)_exchangeTimestamp];
        _exchangeUpdateTimestamp = [QTExchangeETimestamp fromJSONDictionary:(id)_exchangeUpdateTimestamp];
        _filledQuantity = [QTAveragePrice fromJSONDictionary:(id)_filledQuantity];
        _guid = [QTAveragePrice fromJSONDictionary:(id)_guid];
        _instrumentToken = [QTAveragePrice fromJSONDictionary:(id)_instrumentToken];
        _marketProtection = [QTAveragePrice fromJSONDictionary:(id)_marketProtection];
        _meta = [QTMeta fromJSONDictionary:(id)_meta];
        _modified = [QTAveragePrice fromJSONDictionary:(id)_modified];
        _orderID = [QTAveragePrice fromJSONDictionary:(id)_orderID];
        _orderTimestamp = [QTOrderTimestamp fromJSONDictionary:(id)_orderTimestamp];
        _orderType = [QTAveragePrice fromJSONDictionary:(id)_orderType];
        _parentOrderID = [QTAveragePrice fromJSONDictionary:(id)_parentOrderID];
        _pendingQuantity = [QTAveragePrice fromJSONDictionary:(id)_pendingQuantity];
        _placedBy = [QTAveragePrice fromJSONDictionary:(id)_placedBy];
        _price = [QTAveragePrice fromJSONDictionary:(id)_price];
        _product = [QTAveragePrice fromJSONDictionary:(id)_product];
        _quantity = [QTAveragePrice fromJSONDictionary:(id)_quantity];
        _status = [QTAveragePrice fromJSONDictionary:(id)_status];
        _statusMessage = [QTExchangeOrderID fromJSONDictionary:(id)_statusMessage];
        _statusMessageRaw = [QTExchangeOrderID fromJSONDictionary:(id)_statusMessageRaw];
        _tag = [QTExchangeOrderID fromJSONDictionary:(id)_tag];
        _tags = [QTTags fromJSONDictionary:(id)_tags];
        _tradingsymbol = [QTAveragePrice fromJSONDictionary:(id)_tradingsymbol];
        _transactionType = [QTAveragePrice fromJSONDictionary:(id)_transactionType];
        _triggerPrice = [QTAveragePrice fromJSONDictionary:(id)_triggerPrice];
        _validity = [QTAveragePrice fromJSONDictionary:(id)_validity];
        _validityTTL = [QTAveragePrice fromJSONDictionary:(id)_validityTTL];
        _variety = [QTAveragePrice fromJSONDictionary:(id)_variety];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTDatumProperties.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTDatumProperties.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTDatumProperties.properties.allValues] mutableCopy];

    for (id jsonName in QTDatumProperties.properties) {
        id propertyName = QTDatumProperties.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    [dict addEntriesFromDictionary:@{
        @"average_price": [_averagePrice JSONDictionary],
        @"cancelled_quantity": [_cancelledQuantity JSONDictionary],
        @"disclosed_quantity": [_disclosedQuantity JSONDictionary],
        @"exchange": [_exchange JSONDictionary],
        @"exchange_order_id": [_exchangeOrderID JSONDictionary],
        @"exchange_timestamp": [_exchangeTimestamp JSONDictionary],
        @"exchange_update_timestamp": [_exchangeUpdateTimestamp JSONDictionary],
        @"filled_quantity": [_filledQuantity JSONDictionary],
        @"guid": [_guid JSONDictionary],
        @"instrument_token": [_instrumentToken JSONDictionary],
        @"market_protection": [_marketProtection JSONDictionary],
        @"meta": [_meta JSONDictionary],
        @"modified": [_modified JSONDictionary],
        @"order_id": [_orderID JSONDictionary],
        @"order_timestamp": [_orderTimestamp JSONDictionary],
        @"order_type": [_orderType JSONDictionary],
        @"parent_order_id": [_parentOrderID JSONDictionary],
        @"pending_quantity": [_pendingQuantity JSONDictionary],
        @"placed_by": [_placedBy JSONDictionary],
        @"price": [_price JSONDictionary],
        @"product": [_product JSONDictionary],
        @"quantity": [_quantity JSONDictionary],
        @"status": [_status JSONDictionary],
        @"status_message": [_statusMessage JSONDictionary],
        @"status_message_raw": [_statusMessageRaw JSONDictionary],
        @"tag": [_tag JSONDictionary],
        @"tags": [_tags JSONDictionary],
        @"tradingsymbol": [_tradingsymbol JSONDictionary],
        @"transaction_type": [_transactionType JSONDictionary],
        @"trigger_price": [_triggerPrice JSONDictionary],
        @"validity": [_validity JSONDictionary],
        @"validity_ttl": [_validityTTL JSONDictionary],
        @"variety": [_variety JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTAveragePrice
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"type": @"type",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTAveragePrice alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _type = [QTType withValue:(id)_type];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTAveragePrice.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTAveragePrice.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTAveragePrice.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"type": [_type value],
    }];

    return dict;
}
@end

@implementation QTExchangeOrderID
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"anyOf": @"anyOf",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTExchangeOrderID alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _anyOf = map(_anyOf, λ(id x, [QTAveragePrice fromJSONDictionary:x]));
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTExchangeOrderID.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTExchangeOrderID.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTExchangeOrderID.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"anyOf": map(_anyOf, λ(id x, [x JSONDictionary])),
    }];

    return dict;
}
@end

@implementation QTExchangeETimestamp
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"anyOf": @"anyOf",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTExchangeETimestamp alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _anyOf = map(_anyOf, λ(id x, [QTOrderTimestamp fromJSONDictionary:x]));
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTExchangeETimestamp.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTExchangeETimestamp.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTExchangeETimestamp.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"anyOf": map(_anyOf, λ(id x, [x JSONDictionary])),
    }];

    return dict;
}
@end

@implementation QTOrderTimestamp
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"format": @"format",
        @"type": @"type",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTOrderTimestamp alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _type = [QTType withValue:(id)_type];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTOrderTimestamp.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTOrderTimestamp.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTOrderTimestamp.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"type": [_type value],
    }];

    return dict;
}
@end

@implementation QTMeta
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"$ref": @"ref",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTMeta alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTMeta.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTMeta.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTMeta.properties.allValues] mutableCopy];

    for (id jsonName in QTMeta.properties) {
        id propertyName = QTMeta.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    return dict;
}
@end

@implementation QTTags
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"items": @"items",
        @"type": @"type",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTTags alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _items = [QTAveragePrice fromJSONDictionary:(id)_items];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTTags.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTTags.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTTags.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"items": [_items JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTIceberg
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"additionalProperties": @"isAdditionalProperties",
        @"properties": @"properties",
        @"required": @"required",
        @"title": @"title",
        @"type": @"type",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTIceberg alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _properties = [QTIcebergProperties fromJSONDictionary:(id)_properties];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTIceberg.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTIceberg.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTIceberg.properties.allValues] mutableCopy];

    for (id jsonName in QTIceberg.properties) {
        id propertyName = QTIceberg.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    [dict addEntriesFromDictionary:@{
        @"additionalProperties": _isAdditionalProperties ? @YES : @NO,
        @"properties": [_properties JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTIcebergProperties
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"leg": @"leg",
        @"leg_quantity": @"legQuantity",
        @"legs": @"legs",
        @"remaining_quantity": @"remainingQuantity",
        @"total_quantity": @"totalQuantity",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTIcebergProperties alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _leg = [QTAveragePrice fromJSONDictionary:(id)_leg];
        _legQuantity = [QTAveragePrice fromJSONDictionary:(id)_legQuantity];
        _legs = [QTAveragePrice fromJSONDictionary:(id)_legs];
        _remainingQuantity = [QTAveragePrice fromJSONDictionary:(id)_remainingQuantity];
        _totalQuantity = [QTAveragePrice fromJSONDictionary:(id)_totalQuantity];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTIcebergProperties.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTIcebergProperties.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTIcebergProperties.properties.allValues] mutableCopy];

    for (id jsonName in QTIcebergProperties.properties) {
        id propertyName = QTIcebergProperties.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    [dict addEntriesFromDictionary:@{
        @"leg": [_leg JSONDictionary],
        @"leg_quantity": [_legQuantity JSONDictionary],
        @"legs": [_legs JSONDictionary],
        @"remaining_quantity": [_remainingQuantity JSONDictionary],
        @"total_quantity": [_totalQuantity JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTMetaClass
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"additionalProperties": @"isAdditionalProperties",
        @"properties": @"properties",
        @"required": @"required",
        @"title": @"title",
        @"type": @"type",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTMetaClass alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _properties = [QTMetaProperties fromJSONDictionary:(id)_properties];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTMetaClass.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTMetaClass.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTMetaClass.properties.allValues] mutableCopy];

    for (id jsonName in QTMetaClass.properties) {
        id propertyName = QTMetaClass.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    [dict addEntriesFromDictionary:@{
        @"additionalProperties": _isAdditionalProperties ? @YES : @NO,
        @"properties": [_properties JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTMetaProperties
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"iceberg": @"iceberg",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTMetaProperties alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _iceberg = [QTMeta fromJSONDictionary:(id)_iceberg];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTMetaProperties.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTMetaProperties.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTMetaProperties.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"iceberg": [_iceberg JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTOrdersClass
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"additionalProperties": @"isAdditionalProperties",
        @"properties": @"properties",
        @"required": @"required",
        @"title": @"title",
        @"type": @"type",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTOrdersClass alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _properties = [QTOrdersProperties fromJSONDictionary:(id)_properties];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTOrdersClass.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTOrdersClass.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTOrdersClass.properties.allValues] mutableCopy];

    for (id jsonName in QTOrdersClass.properties) {
        id propertyName = QTOrdersClass.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    [dict addEntriesFromDictionary:@{
        @"additionalProperties": _isAdditionalProperties ? @YES : @NO,
        @"properties": [_properties JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTOrdersProperties
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"data": @"data",
        @"status": @"status",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTOrdersProperties alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _data = [QTData fromJSONDictionary:(id)_data];
        _status = [QTAveragePrice fromJSONDictionary:(id)_status];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTOrdersProperties.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTOrdersProperties.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTOrdersProperties.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"data": [_data JSONDictionary],
        @"status": [_status JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTData
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"items": @"items",
        @"type": @"type",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTData alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _items = [QTMeta fromJSONDictionary:(id)_items];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTData.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTData.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTData.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"items": [_items JSONDictionary],
    }];

    return dict;
}
@end

NS_ASSUME_NONNULL_END
