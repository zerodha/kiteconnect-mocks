#import "postback.h"

#define λ(decl, expr) (^(decl) { return (expr); })

static id NSNullify(id _Nullable x) {
    return (x == nil || x == NSNull.null) ? NSNull.null : x;
}

NS_ASSUME_NONNULL_BEGIN

@interface QTPostback (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTDefinitions (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTMeta (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTPostbackClass (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTProperties (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTAppID (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTTimestamp (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTMetaClass (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@implementation QTType
+ (NSDictionary<NSString *, QTType *> *)values
{
    static NSDictionary<NSString *, QTType *> *values;
    return values = values ? values : @{
        @"integer": [[QTType alloc] initWithValue:@"integer"],
        @"null": [[QTType alloc] initWithValue:@"null"],
        @"string": [[QTType alloc] initWithValue:@"string"],
    };
}

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

QTPostback *_Nullable QTPostbackFromData(NSData *data, NSError **error)
{
    @try {
        id json = [NSJSONSerialization JSONObjectWithData:data options:NSJSONReadingAllowFragments error:error];
        return *error ? nil : [QTPostback fromJSONDictionary:json];
    } @catch (NSException *exception) {
        *error = [NSError errorWithDomain:@"JSONSerialization" code:-1 userInfo:@{ @"exception": exception }];
        return nil;
    }
}

QTPostback *_Nullable QTPostbackFromJSON(NSString *json, NSStringEncoding encoding, NSError **error)
{
    return QTPostbackFromData([json dataUsingEncoding:encoding], error);
}

NSData *_Nullable QTPostbackToData(QTPostback *postback, NSError **error)
{
    @try {
        id json = [postback JSONDictionary];
        NSData *data = [NSJSONSerialization dataWithJSONObject:json options:kNilOptions error:error];
        return *error ? nil : data;
    } @catch (NSException *exception) {
        *error = [NSError errorWithDomain:@"JSONSerialization" code:-1 userInfo:@{ @"exception": exception }];
        return nil;
    }
}

NSString *_Nullable QTPostbackToJSON(QTPostback *postback, NSStringEncoding encoding, NSError **error)
{
    NSData *data = QTPostbackToData(postback, error);
    return data ? [[NSString alloc] initWithData:data encoding:encoding] : nil;
}

@implementation QTPostback
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
    return QTPostbackFromData(data, error);
}

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error
{
    return QTPostbackFromJSON(json, encoding, error);
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTPostback alloc] initWithJSONDictionary:dict] : nil;
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
    id resolved = QTPostback.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTPostback.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTPostback.properties.allValues] mutableCopy];

    for (id jsonName in QTPostback.properties) {
        id propertyName = QTPostback.properties[jsonName];
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
    return QTPostbackToData(self, error);
}

- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error
{
    return QTPostbackToJSON(self, encoding, error);
}
@end

@implementation QTDefinitions
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"Meta": @"meta",
        @"Postback": @"postback",
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
        _meta = [QTMeta fromJSONDictionary:(id)_meta];
        _postback = [QTPostbackClass fromJSONDictionary:(id)_postback];
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
        @"Meta": [_meta JSONDictionary],
        @"Postback": [_postback JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTMeta
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"additionalProperties": @"isAdditionalProperties",
        @"title": @"title",
        @"type": @"type",
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

    [dict addEntriesFromDictionary:@{
        @"additionalProperties": _isAdditionalProperties ? @YES : @NO,
    }];

    return dict;
}
@end

@implementation QTPostbackClass
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
    return dict ? [[QTPostbackClass alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _properties = [QTProperties fromJSONDictionary:(id)_properties];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTPostbackClass.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTPostbackClass.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTPostbackClass.properties.allValues] mutableCopy];

    for (id jsonName in QTPostbackClass.properties) {
        id propertyName = QTPostbackClass.properties[jsonName];
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

@implementation QTProperties
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"app_id": @"appID",
        @"average_price": @"averagePrice",
        @"cancelled_quantity": @"cancelledQuantity",
        @"checksum": @"checksum",
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
        @"tradingsymbol": @"tradingsymbol",
        @"transaction_type": @"transactionType",
        @"trigger_price": @"triggerPrice",
        @"unfilled_quantity": @"unfilledQuantity",
        @"user_id": @"userID",
        @"validity": @"validity",
        @"variety": @"variety",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTProperties alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _appID = [QTAppID fromJSONDictionary:(id)_appID];
        _averagePrice = [QTAppID fromJSONDictionary:(id)_averagePrice];
        _cancelledQuantity = [QTAppID fromJSONDictionary:(id)_cancelledQuantity];
        _checksum = [QTAppID fromJSONDictionary:(id)_checksum];
        _disclosedQuantity = [QTAppID fromJSONDictionary:(id)_disclosedQuantity];
        _exchange = [QTAppID fromJSONDictionary:(id)_exchange];
        _exchangeOrderID = [QTAppID fromJSONDictionary:(id)_exchangeOrderID];
        _exchangeTimestamp = [QTTimestamp fromJSONDictionary:(id)_exchangeTimestamp];
        _exchangeUpdateTimestamp = [QTTimestamp fromJSONDictionary:(id)_exchangeUpdateTimestamp];
        _filledQuantity = [QTAppID fromJSONDictionary:(id)_filledQuantity];
        _guid = [QTAppID fromJSONDictionary:(id)_guid];
        _instrumentToken = [QTAppID fromJSONDictionary:(id)_instrumentToken];
        _marketProtection = [QTAppID fromJSONDictionary:(id)_marketProtection];
        _meta = [QTMetaClass fromJSONDictionary:(id)_meta];
        _orderID = [QTAppID fromJSONDictionary:(id)_orderID];
        _orderTimestamp = [QTTimestamp fromJSONDictionary:(id)_orderTimestamp];
        _orderType = [QTAppID fromJSONDictionary:(id)_orderType];
        _parentOrderID = [QTAppID fromJSONDictionary:(id)_parentOrderID];
        _pendingQuantity = [QTAppID fromJSONDictionary:(id)_pendingQuantity];
        _placedBy = [QTAppID fromJSONDictionary:(id)_placedBy];
        _price = [QTAppID fromJSONDictionary:(id)_price];
        _product = [QTAppID fromJSONDictionary:(id)_product];
        _quantity = [QTAppID fromJSONDictionary:(id)_quantity];
        _status = [QTAppID fromJSONDictionary:(id)_status];
        _statusMessage = [QTAppID fromJSONDictionary:(id)_statusMessage];
        _statusMessageRaw = [QTAppID fromJSONDictionary:(id)_statusMessageRaw];
        _tag = [QTAppID fromJSONDictionary:(id)_tag];
        _tradingsymbol = [QTAppID fromJSONDictionary:(id)_tradingsymbol];
        _transactionType = [QTAppID fromJSONDictionary:(id)_transactionType];
        _triggerPrice = [QTAppID fromJSONDictionary:(id)_triggerPrice];
        _unfilledQuantity = [QTAppID fromJSONDictionary:(id)_unfilledQuantity];
        _userID = [QTAppID fromJSONDictionary:(id)_userID];
        _validity = [QTAppID fromJSONDictionary:(id)_validity];
        _variety = [QTAppID fromJSONDictionary:(id)_variety];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTProperties.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTProperties.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTProperties.properties.allValues] mutableCopy];

    for (id jsonName in QTProperties.properties) {
        id propertyName = QTProperties.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    [dict addEntriesFromDictionary:@{
        @"app_id": [_appID JSONDictionary],
        @"average_price": [_averagePrice JSONDictionary],
        @"cancelled_quantity": [_cancelledQuantity JSONDictionary],
        @"checksum": [_checksum JSONDictionary],
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
        @"tradingsymbol": [_tradingsymbol JSONDictionary],
        @"transaction_type": [_transactionType JSONDictionary],
        @"trigger_price": [_triggerPrice JSONDictionary],
        @"unfilled_quantity": [_unfilledQuantity JSONDictionary],
        @"user_id": [_userID JSONDictionary],
        @"validity": [_validity JSONDictionary],
        @"variety": [_variety JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTAppID
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"type": @"type",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTAppID alloc] initWithJSONDictionary:dict] : nil;
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
    id resolved = QTAppID.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTAppID.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTAppID.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"type": [_type value],
    }];

    return dict;
}
@end

@implementation QTTimestamp
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
    return dict ? [[QTTimestamp alloc] initWithJSONDictionary:dict] : nil;
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
    id resolved = QTTimestamp.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTTimestamp.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTTimestamp.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"type": [_type value],
    }];

    return dict;
}
@end

@implementation QTMetaClass
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"$ref": @"ref",
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

    return dict;
}
@end

NS_ASSUME_NONNULL_END
