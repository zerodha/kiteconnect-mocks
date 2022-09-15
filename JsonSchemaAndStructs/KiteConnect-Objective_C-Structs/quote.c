#import "quote.h"

#define λ(decl, expr) (^(decl) { return (expr); })

static id NSNullify(id _Nullable x) {
    return (x == nil || x == NSNull.null) ? NSNull.null : x;
}

NS_ASSUME_NONNULL_BEGIN

@interface QTQuote (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTDefinitions (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTBuy (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTBuyProperties (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTOrders (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTData (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTDataProperties (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTNseInfy (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTDepth (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTDepthProperties (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTBuyClass (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTNseInfyClass (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTNseInfyProperties (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTLastTradeTime (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTOhlc (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTOhlcProperties (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTQuoteClass (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@interface QTQuoteProperties (JSONConversion)
+ (instancetype)fromJSONDictionary:(NSDictionary *)dict;
- (NSDictionary *)JSONDictionary;
@end

@implementation QTType
+ (NSDictionary<NSString *, QTType *> *)values
{
    static NSDictionary<NSString *, QTType *> *values;
    return values = values ? values : @{
        @"integer": [[QTType alloc] initWithValue:@"integer"],
        @"number": [[QTType alloc] initWithValue:@"number"],
        @"string": [[QTType alloc] initWithValue:@"string"],
    };
}

+ (QTType *)integer { return QTType.values[@"integer"]; }
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

QTQuote *_Nullable QTQuoteFromData(NSData *data, NSError **error)
{
    @try {
        id json = [NSJSONSerialization JSONObjectWithData:data options:NSJSONReadingAllowFragments error:error];
        return *error ? nil : [QTQuote fromJSONDictionary:json];
    } @catch (NSException *exception) {
        *error = [NSError errorWithDomain:@"JSONSerialization" code:-1 userInfo:@{ @"exception": exception }];
        return nil;
    }
}

QTQuote *_Nullable QTQuoteFromJSON(NSString *json, NSStringEncoding encoding, NSError **error)
{
    return QTQuoteFromData([json dataUsingEncoding:encoding], error);
}

NSData *_Nullable QTQuoteToData(QTQuote *quote, NSError **error)
{
    @try {
        id json = [quote JSONDictionary];
        NSData *data = [NSJSONSerialization dataWithJSONObject:json options:kNilOptions error:error];
        return *error ? nil : data;
    } @catch (NSException *exception) {
        *error = [NSError errorWithDomain:@"JSONSerialization" code:-1 userInfo:@{ @"exception": exception }];
        return nil;
    }
}

NSString *_Nullable QTQuoteToJSON(QTQuote *quote, NSStringEncoding encoding, NSError **error)
{
    NSData *data = QTQuoteToData(quote, error);
    return data ? [[NSString alloc] initWithData:data encoding:encoding] : nil;
}

@implementation QTQuote
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
    return QTQuoteFromData(data, error);
}

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error
{
    return QTQuoteFromJSON(json, encoding, error);
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTQuote alloc] initWithJSONDictionary:dict] : nil;
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
    id resolved = QTQuote.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTQuote.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTQuote.properties.allValues] mutableCopy];

    for (id jsonName in QTQuote.properties) {
        id propertyName = QTQuote.properties[jsonName];
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
    return QTQuoteToData(self, error);
}

- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error
{
    return QTQuoteToJSON(self, encoding, error);
}
@end

@implementation QTDefinitions
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"Buy": @"buy",
        @"Data": @"data",
        @"Depth": @"depth",
        @"NseInfy": @"nseInfy",
        @"Ohlc": @"ohlc",
        @"Quote": @"quote",
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
        _buy = [QTBuy fromJSONDictionary:(id)_buy];
        _data = [QTData fromJSONDictionary:(id)_data];
        _depth = [QTDepth fromJSONDictionary:(id)_depth];
        _nseInfy = [QTNseInfyClass fromJSONDictionary:(id)_nseInfy];
        _ohlc = [QTOhlc fromJSONDictionary:(id)_ohlc];
        _quote = [QTQuoteClass fromJSONDictionary:(id)_quote];
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
        @"Buy": [_buy JSONDictionary],
        @"Data": [_data JSONDictionary],
        @"Depth": [_depth JSONDictionary],
        @"NseInfy": [_nseInfy JSONDictionary],
        @"Ohlc": [_ohlc JSONDictionary],
        @"Quote": [_quote JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTBuy
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
    return dict ? [[QTBuy alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _properties = [QTBuyProperties fromJSONDictionary:(id)_properties];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTBuy.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTBuy.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTBuy.properties.allValues] mutableCopy];

    for (id jsonName in QTBuy.properties) {
        id propertyName = QTBuy.properties[jsonName];
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

@implementation QTBuyProperties
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"orders": @"orders",
        @"price": @"price",
        @"quantity": @"quantity",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTBuyProperties alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _orders = [QTOrders fromJSONDictionary:(id)_orders];
        _price = [QTOrders fromJSONDictionary:(id)_price];
        _quantity = [QTOrders fromJSONDictionary:(id)_quantity];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTBuyProperties.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTBuyProperties.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTBuyProperties.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"orders": [_orders JSONDictionary],
        @"price": [_price JSONDictionary],
        @"quantity": [_quantity JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTOrders
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"type": @"type",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTOrders alloc] initWithJSONDictionary:dict] : nil;
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

    [dict addEntriesFromDictionary:@{
        @"type": [_type value],
    }];

    return dict;
}
@end

@implementation QTData
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
    return dict ? [[QTData alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _properties = [QTDataProperties fromJSONDictionary:(id)_properties];
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

    for (id jsonName in QTData.properties) {
        id propertyName = QTData.properties[jsonName];
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

@implementation QTDataProperties
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"NSE:INFY": @"nseInfy",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTDataProperties alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _nseInfy = [QTNseInfy fromJSONDictionary:(id)_nseInfy];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTDataProperties.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTDataProperties.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTDataProperties.properties.allValues] mutableCopy];

    for (id jsonName in QTDataProperties.properties) {
        id propertyName = QTDataProperties.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    [dict addEntriesFromDictionary:@{
        @"NSE:INFY": [_nseInfy JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTNseInfy
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"$ref": @"ref",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTNseInfy alloc] initWithJSONDictionary:dict] : nil;
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
    id resolved = QTNseInfy.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTNseInfy.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTNseInfy.properties.allValues] mutableCopy];

    for (id jsonName in QTNseInfy.properties) {
        id propertyName = QTNseInfy.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    return dict;
}
@end

@implementation QTDepth
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
    return dict ? [[QTDepth alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _properties = [QTDepthProperties fromJSONDictionary:(id)_properties];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTDepth.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTDepth.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTDepth.properties.allValues] mutableCopy];

    for (id jsonName in QTDepth.properties) {
        id propertyName = QTDepth.properties[jsonName];
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

@implementation QTDepthProperties
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"buy": @"buy",
        @"sell": @"sell",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTDepthProperties alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _buy = [QTBuyClass fromJSONDictionary:(id)_buy];
        _sell = [QTBuyClass fromJSONDictionary:(id)_sell];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTDepthProperties.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTDepthProperties.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTDepthProperties.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"buy": [_buy JSONDictionary],
        @"sell": [_sell JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTBuyClass
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
    return dict ? [[QTBuyClass alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _items = [QTNseInfy fromJSONDictionary:(id)_items];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTBuyClass.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTBuyClass.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTBuyClass.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"items": [_items JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTNseInfyClass
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
    return dict ? [[QTNseInfyClass alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _properties = [QTNseInfyProperties fromJSONDictionary:(id)_properties];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTNseInfyClass.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTNseInfyClass.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTNseInfyClass.properties.allValues] mutableCopy];

    for (id jsonName in QTNseInfyClass.properties) {
        id propertyName = QTNseInfyClass.properties[jsonName];
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

@implementation QTNseInfyProperties
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"average_price": @"averagePrice",
        @"buy_quantity": @"buyQuantity",
        @"depth": @"depth",
        @"instrument_token": @"instrumentToken",
        @"last_price": @"lastPrice",
        @"last_quantity": @"lastQuantity",
        @"last_trade_time": @"lastTradeTime",
        @"lower_circuit_limit": @"lowerCircuitLimit",
        @"net_change": @"netChange",
        @"ohlc": @"ohlc",
        @"oi": @"oi",
        @"oi_day_high": @"oiDayHigh",
        @"oi_day_low": @"oiDayLow",
        @"sell_quantity": @"sellQuantity",
        @"timestamp": @"timestamp",
        @"upper_circuit_limit": @"upperCircuitLimit",
        @"volume": @"volume",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTNseInfyProperties alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _averagePrice = [QTOrders fromJSONDictionary:(id)_averagePrice];
        _buyQuantity = [QTOrders fromJSONDictionary:(id)_buyQuantity];
        _depth = [QTNseInfy fromJSONDictionary:(id)_depth];
        _instrumentToken = [QTOrders fromJSONDictionary:(id)_instrumentToken];
        _lastPrice = [QTOrders fromJSONDictionary:(id)_lastPrice];
        _lastQuantity = [QTOrders fromJSONDictionary:(id)_lastQuantity];
        _lastTradeTime = [QTLastTradeTime fromJSONDictionary:(id)_lastTradeTime];
        _lowerCircuitLimit = [QTOrders fromJSONDictionary:(id)_lowerCircuitLimit];
        _netChange = [QTOrders fromJSONDictionary:(id)_netChange];
        _ohlc = [QTNseInfy fromJSONDictionary:(id)_ohlc];
        _oi = [QTOrders fromJSONDictionary:(id)_oi];
        _oiDayHigh = [QTOrders fromJSONDictionary:(id)_oiDayHigh];
        _oiDayLow = [QTOrders fromJSONDictionary:(id)_oiDayLow];
        _sellQuantity = [QTOrders fromJSONDictionary:(id)_sellQuantity];
        _timestamp = [QTLastTradeTime fromJSONDictionary:(id)_timestamp];
        _upperCircuitLimit = [QTOrders fromJSONDictionary:(id)_upperCircuitLimit];
        _volume = [QTOrders fromJSONDictionary:(id)_volume];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTNseInfyProperties.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTNseInfyProperties.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTNseInfyProperties.properties.allValues] mutableCopy];

    for (id jsonName in QTNseInfyProperties.properties) {
        id propertyName = QTNseInfyProperties.properties[jsonName];
        if (![jsonName isEqualToString:propertyName]) {
            dict[jsonName] = dict[propertyName];
            [dict removeObjectForKey:propertyName];
        }
    }

    [dict addEntriesFromDictionary:@{
        @"average_price": [_averagePrice JSONDictionary],
        @"buy_quantity": [_buyQuantity JSONDictionary],
        @"depth": [_depth JSONDictionary],
        @"instrument_token": [_instrumentToken JSONDictionary],
        @"last_price": [_lastPrice JSONDictionary],
        @"last_quantity": [_lastQuantity JSONDictionary],
        @"last_trade_time": [_lastTradeTime JSONDictionary],
        @"lower_circuit_limit": [_lowerCircuitLimit JSONDictionary],
        @"net_change": [_netChange JSONDictionary],
        @"ohlc": [_ohlc JSONDictionary],
        @"oi": [_oi JSONDictionary],
        @"oi_day_high": [_oiDayHigh JSONDictionary],
        @"oi_day_low": [_oiDayLow JSONDictionary],
        @"sell_quantity": [_sellQuantity JSONDictionary],
        @"timestamp": [_timestamp JSONDictionary],
        @"upper_circuit_limit": [_upperCircuitLimit JSONDictionary],
        @"volume": [_volume JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTLastTradeTime
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
    return dict ? [[QTLastTradeTime alloc] initWithJSONDictionary:dict] : nil;
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
    id resolved = QTLastTradeTime.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTLastTradeTime.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTLastTradeTime.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"type": [_type value],
    }];

    return dict;
}
@end

@implementation QTOhlc
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
    return dict ? [[QTOhlc alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _properties = [QTOhlcProperties fromJSONDictionary:(id)_properties];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTOhlc.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTOhlc.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTOhlc.properties.allValues] mutableCopy];

    for (id jsonName in QTOhlc.properties) {
        id propertyName = QTOhlc.properties[jsonName];
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

@implementation QTOhlcProperties
+ (NSDictionary<NSString *, NSString *> *)properties
{
    static NSDictionary<NSString *, NSString *> *properties;
    return properties = properties ? properties : @{
        @"close": @"close",
        @"high": @"high",
        @"low": @"low",
        @"open": @"open",
    };
}

+ (instancetype)fromJSONDictionary:(NSDictionary *)dict
{
    return dict ? [[QTOhlcProperties alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _close = [QTOrders fromJSONDictionary:(id)_close];
        _high = [QTOrders fromJSONDictionary:(id)_high];
        _low = [QTOrders fromJSONDictionary:(id)_low];
        _open = [QTOrders fromJSONDictionary:(id)_open];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTOhlcProperties.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTOhlcProperties.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTOhlcProperties.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"close": [_close JSONDictionary],
        @"high": [_high JSONDictionary],
        @"low": [_low JSONDictionary],
        @"open": [_open JSONDictionary],
    }];

    return dict;
}
@end

@implementation QTQuoteClass
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
    return dict ? [[QTQuoteClass alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _properties = [QTQuoteProperties fromJSONDictionary:(id)_properties];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTQuoteClass.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTQuoteClass.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTQuoteClass.properties.allValues] mutableCopy];

    for (id jsonName in QTQuoteClass.properties) {
        id propertyName = QTQuoteClass.properties[jsonName];
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

@implementation QTQuoteProperties
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
    return dict ? [[QTQuoteProperties alloc] initWithJSONDictionary:dict] : nil;
}

- (instancetype)initWithJSONDictionary:(NSDictionary *)dict
{
    if (self = [super init]) {
        [self setValuesForKeysWithDictionary:dict];
        _data = [QTNseInfy fromJSONDictionary:(id)_data];
        _status = [QTOrders fromJSONDictionary:(id)_status];
    }
    return self;
}

- (void)setValue:(nullable id)value forKey:(NSString *)key
{
    id resolved = QTQuoteProperties.properties[key];
    if (resolved) [super setValue:value forKey:resolved];
}

- (void)setNilValueForKey:(NSString *)key
{
    id resolved = QTQuoteProperties.properties[key];
    if (resolved) [super setValue:@(0) forKey:resolved];
}

- (NSDictionary *)JSONDictionary
{
    id dict = [[self dictionaryWithValuesForKeys:QTQuoteProperties.properties.allValues] mutableCopy];

    [dict addEntriesFromDictionary:@{
        @"data": [_data JSONDictionary],
        @"status": [_status JSONDictionary],
    }];

    return dict;
}
@end

NS_ASSUME_NONNULL_END
