// To parse this JSON:
//
//   NSError *error;
//   QTTrades *trades = [QTTrades fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTTrades;
@class QTDefinitions;
@class QTDatum;
@class QTDatumProperties;
@class QTAveragePrice;
@class QTType;
@class QTExchangeTimestamp;
@class QTTradesClass;
@class QTTradesProperties;
@class QTData;
@class QTItems;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Boxed enums

@interface QTType : NSObject
@property (nonatomic, readonly, copy) NSString *value;
+ (instancetype _Nullable)withValue:(NSString *)value;
+ (QTType *)integer;
+ (QTType *)number;
+ (QTType *)string;
@end

#pragma mark - Object interfaces

@interface QTTrades : NSObject
@property (nonatomic, copy)   NSString *ref;
@property (nonatomic, copy)   NSString *schema;
@property (nonatomic, strong) QTDefinitions *definitions;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface QTDefinitions : NSObject
@property (nonatomic, strong) QTDatum *datum;
@property (nonatomic, strong) QTTradesClass *trades;
@end

@interface QTDatum : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDatumProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDatumProperties : NSObject
@property (nonatomic, strong) QTAveragePrice *averagePrice;
@property (nonatomic, strong) QTAveragePrice *exchange;
@property (nonatomic, strong) QTAveragePrice *exchangeOrderID;
@property (nonatomic, strong) QTExchangeTimestamp *exchangeTimestamp;
@property (nonatomic, strong) QTExchangeTimestamp *fillTimestamp;
@property (nonatomic, strong) QTAveragePrice *instrumentToken;
@property (nonatomic, strong) QTAveragePrice *orderID;
@property (nonatomic, strong) QTExchangeTimestamp *orderTimestamp;
@property (nonatomic, strong) QTAveragePrice *product;
@property (nonatomic, strong) QTAveragePrice *quantity;
@property (nonatomic, strong) QTExchangeTimestamp *tradeID;
@property (nonatomic, strong) QTAveragePrice *tradingsymbol;
@property (nonatomic, strong) QTAveragePrice *transactionType;
@end

@interface QTAveragePrice : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTExchangeTimestamp : NSObject
@property (nonatomic, copy)   NSString *format;
@property (nonatomic, assign) QTType *type;
@end

@interface QTTradesClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTTradesProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTTradesProperties : NSObject
@property (nonatomic, strong) QTData *data;
@property (nonatomic, strong) QTAveragePrice *status;
@end

@interface QTData : NSObject
@property (nonatomic, strong) QTItems *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTItems : NSObject
@property (nonatomic, copy) NSString *ref;
@end

NS_ASSUME_NONNULL_END
