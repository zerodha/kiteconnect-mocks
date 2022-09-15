#import "mf_orders.h"

#define λ(decl, expr) (^(decl) { return (expr); })

static id NSNullify(id _Nullable x) {
    return (x == nil || x == NSNull.null) ? NSNull.null : x;
}

NS_ASSUME_NONNULL_BEGIN

@interface QTMFOrders (JSONConversion)
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

@interface QTAmount (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTExchangeOrderID (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTLastPriceDate (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTTag (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTMFOrdersClass (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTMFOrdersProperties (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTData (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTItems (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@implementation QTType
+ (NSDictionary<NSString *, QTType *> *)values
{
    static NSDictionary<NSString *, QTType *> *values;
    return values = values ? values : @{
        @"null": [[QTType alloc] initWithValue:@"null"],
        @"number": [[QTType alloc] initWithValue:@"number"],
        @"string": [[QTType alloc] initWithValue:@"string"],
    };
}

+ (QTType *)null { return QTType.values[@"null"]; }
+ (QTType *)number { return QTType.values[@"number"]; }
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

QTMFOrders *_Nullable QTMFOrdersFromData(NSData *data, NSError **error)
{
    @try {
        id json = [NSJSONSerialization JSONObjectWithData:data options:NSJSONReadingAllowFragments error:error];
        return *error ? nil : [QTMFOrders fromJSONDictionary:json];
    } @catch (NSException *exception) {
        *error = [NSError errorWithDomain:@"JSONSerialization" code:-1 userInfo:@{ @"exception": exception }];
        return nil;
    }
}

QTMFOrders *_Nullable QTMFOrdersFromJSON(NSString *json, NSStringEncoding encoding, NSError **error)
{
    return QTMFOrdersFromData([json dataUsingEncoding:encoding], error);
}

NSData *_Nullable QTMFOrdersToData(QTMFOrders *mFOrders, NSError **error)
{
    @try {
        id json = [mFOrders JSONDictionary];
        NSData *data = [NSJSONSerialization dataWithJSONObject:json options:kNilOptions error:error];
        return *error ? nil : data;
    } @catch (NSException *exception) {
        *error = [NSError errorWithDomain:@"JSONSerialization" code:-1 userInfo:@{ @"exception": exception }];
        return nil;
    }
}

NSString *_Nullable QTMFOrdersToJSON(QTMFOrders *mFOrders, NSStringEncoding encoding, NSError **error)
{
    NSData *data = QTMFOrdersToData(mFOrders, error);
    return data ? [[NSString alloc] initWithData:data encoding:encoding] : nil;
}

@implementation QTMFOrders
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
    return QTMFOrdersFromData(data, error);
}

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error
{
    return QTMFOrdersFromJSON(json, encoding, error);
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTMFOrders alloc] initWithJSONDictionary:dict] : nil;
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
    id resolved = QTMFOrders.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTMFOrders.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTMFOrders.properties.allValues] mutableCopy];

    for (id jsonName in QTMFOrders.properties) {
        id propertyName = QTMFOrders.properties[jsonName];
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
    return QTMFOrdersToData(self, error);
}

- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error
{
    return QTMFOrdersToJSON(self, encoding, error);
}
@end

@implementation QTDefinitions
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"Datum": @"datum",
        @"MFOrders": @"mfOrders",
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
        _mfOrders = [QTMFOrdersClass fromJSONDictionary:(id)_mfOrders];
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
        @"MFOrders": [_mfOrders JSONDictionary],
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
        @"amount": @"amount",
        @"average_price": @"averagePrice",
        @"exchange_order_id": @"exchangeOrderID",
        @"exchange_timestamp": @"exchangeTimestamp",
        @"folio": @"folio",
        @"fund": @"fund",
        @"last_price": @"lastPrice",
        @"last_price_date": @"lastPriceDate",
        @"order_id": @"orderID",
        @"order_timestamp": @"orderTimestamp",
        @"placed_by": @"placedBy",
        @"purchase_type": @"purchaseType",
        @"quantity": @"quantity",
        @"settlement_id": @"settlementID",
        @"status": @"status",
        @"status_message": @"statusMessage",
        @"tag": @"tag",
        @"tradingsymbol": @"tradingsymbol",
        @"transaction_type": @"transactionType",
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
        _amount = [QTAmount fromJSONDictionary:(id)_amount];
        _averagePrice = [QTAmount fromJSONDictionary:(id)_averagePrice];
        _exchangeOrderID = [QTExchangeOrderID fromJSONDictionary:(id)_exchangeOrderID];
        _exchangeTimestamp = [QTExchangeOrderID fromJSONDictionary:(id)_exchangeTimestamp];
        _folio = [QTAmount fromJSONDictionary:(id)_folio];
        _fund = [QTAmount fromJSONDictionary:(id)_fund];
        _lastPrice = [QTAmount fromJSONDictionary:(id)_lastPrice];
        _lastPriceDate = [QTLastPriceDate fromJSONDictionary:(id)_lastPriceDate];
        _orderID = [QTLastPriceDate fromJSONDictionary:(id)_orderID];
        _orderTimestamp = [QTLastPriceDate fromJSONDictionary:(id)_orderTimestamp];
        _placedBy = [QTAmount fromJSONDictionary:(id)_placedBy];
        _purchaseType = [QTAmount fromJSONDictionary:(id)_purchaseType];
        _quantity = [QTAmount fromJSONDictionary:(id)_quantity];
        _settlementID = [QTExchangeOrderID fromJSONDictionary:(id)_settlementID];
        _status = [QTAmount fromJSONDictionary:(id)_status];
        _statusMessage = [QTAmount fromJSONDictionary:(id)_statusMessage];
        _tag = [QTTag fromJSONDictionary:(id)_tag];
        _tradingsymbol = [QTAmount fromJSONDictionary:(id)_tradingsymbol];
        _transactionType = [QTAmount fromJSONDictionary:(id)_transactionType];
        _variety = [QTAmount fromJSONDictionary:(id)_variety];
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
        @"amount": [_amount JSONDictionary],
        @"average_price": [_averagePrice JSONDictionary],
        @"exchange_order_id": [_exchangeOrderID JSONDictionary],
        @"exchange_timestamp": [_exchangeTimestamp JSONDictionary],
        @"folio": [_folio JSONDictionary],
        @"fund": [_fund JSONDictionary],
        @"last_price": [_lastPrice JSONDictionary],
        @"last_price_date": [_lastPriceDate JSONDictionary],
        @"order_id": [_orderID JSONDictionary],
        @"order_timestamp": [_orderTimestamp JSONDictionary],
        @"placed_by": [_placedBy JSONDictionary],
        @"purchase_type": [_purchaseType JSONDictionary],
        @"quantity": [_quantity JSONDictionary],
        @"settlement_id": [_settlementID JSONDictionary],
        @"status": [_status JSONDictionary],
        @"status_message": [_statusMessage JSONDictionary],
        @"tag": [_tag JSONDictionary],
        @"tradingsymbol": [_tradingsymbol JSONDictionary],
        @"transaction_type": [_transactionType JSONDictionary],
        @"variety": [_variety JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTAmount
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"type": @"type",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTAmount alloc] initWithJSONDictionary:dict] : nil;
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
    id resolved = QTAmount.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTAmount.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTAmount.properties.allValues] mutableCopy];

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
        _anyOf = map(_anyOf, λ(id x, [QTLastPriceDate fromJSONDictionary:x]));
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

@implementation QTLastPriceDate
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
    return dict ? [[QTLastPriceDate alloc] initWithJSONDictionary:dict] : nil;
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
    id resolved = QTLastPriceDate.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTLastPriceDate.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTLastPriceDate.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"type": [_type value],
    }];

    return dict;
}
@end

@implementation QTTag
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"anyOf": @"anyOf",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTTag alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _anyOf = map(_anyOf, λ(id x, [QTAmount fromJSONDictionary:x]));
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTTag.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTTag.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTTag.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"anyOf": map(_anyOf, λ(id x, [x JSONDictionary])),
    }];

    return dict;
}
@end

@implementation QTMFOrdersClass
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
    return dict ? [[QTMFOrdersClass alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _properties = [QTMFOrdersProperties fromJSONDictionary:(id)_properties];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTMFOrdersClass.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTMFOrdersClass.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTMFOrdersClass.properties.allValues] mutableCopy];

    for (id jsonName in QTMFOrdersClass.properties) {
        id propertyName = QTMFOrdersClass.properties[jsonName];
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

@implementation QTMFOrdersProperties
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
    return dict ? [[QTMFOrdersProperties alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _data = [QTData fromJSONDictionary:(id)_data];
        _status = [QTAmount fromJSONDictionary:(id)_status];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTMFOrdersProperties.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTMFOrdersProperties.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTMFOrdersProperties.properties.allValues] mutableCopy];

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
        _items = [QTItems fromJSONDictionary:(id)_items];
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

@implementation QTItems
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"$ref": @"ref",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTItems alloc] initWithJSONDictionary:dict] : nil;
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
    id resolved = QTItems.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTItems.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTItems.properties.allValues] mutableCopy];

    for (id jsonName in QTItems.properties) {
        id propertyName = QTItems.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    return dict;
}
@end

NS_ASSUME_NONNULL_END
