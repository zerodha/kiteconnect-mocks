// To parse this JSON:
//
//   NSError *error;
//   QTOrders *orders = [QTOrders fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTOrders;
@class QTDefinitions;
@class QTDatum;
@class QTDatumProperties;
@class QTAveragePrice;
@class QTType;
@class QTExchangeOrderID;
@class QTExchangeETimestamp;
@class QTOrderTimestamp;
@class QTMeta;
@class QTTags;
@class QTIceberg;
@class QTIcebergProperties;
@class QTMetaClass;
@class QTMetaProperties;
@class QTOrdersClass;
@class QTOrdersProperties;
@class QTData;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Boxed enums

@interface QTType : NSObject
@property (nonatomic, readonly, copy) NSString *value;
+ (instancetype _Nullable)withValue:(NSString *)value;
+ (QTType *)boolean;
+ (QTType *)integer;
+ (QTType *)null;
+ (QTType *)string;
@end

#pragma mark - Object interfaces

@interface QTOrders : NSObject
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
@property (nonatomic, strong) QTIceberg *iceberg;
@property (nonatomic, strong) QTMetaClass *meta;
@property (nonatomic, strong) QTOrdersClass *orders;
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
@property (nonatomic, strong) QTAveragePrice *cancelledQuantity;
@property (nonatomic, strong) QTAveragePrice *disclosedQuantity;
@property (nonatomic, strong) QTAveragePrice *exchange;
@property (nonatomic, strong) QTExchangeOrderID *exchangeOrderID;
@property (nonatomic, strong) QTExchangeETimestamp *exchangeTimestamp;
@property (nonatomic, strong) QTExchangeETimestamp *exchangeUpdateTimestamp;
@property (nonatomic, strong) QTAveragePrice *filledQuantity;
@property (nonatomic, strong) QTAveragePrice *guid;
@property (nonatomic, strong) QTAveragePrice *instrumentToken;
@property (nonatomic, strong) QTAveragePrice *marketProtection;
@property (nonatomic, strong) QTMeta *meta;
@property (nonatomic, strong) QTAveragePrice *modified;
@property (nonatomic, strong) QTAveragePrice *orderID;
@property (nonatomic, strong) QTOrderTimestamp *orderTimestamp;
@property (nonatomic, strong) QTAveragePrice *orderType;
@property (nonatomic, strong) QTAveragePrice *parentOrderID;
@property (nonatomic, strong) QTAveragePrice *pendingQuantity;
@property (nonatomic, strong) QTAveragePrice *placedBy;
@property (nonatomic, strong) QTAveragePrice *price;
@property (nonatomic, strong) QTAveragePrice *product;
@property (nonatomic, strong) QTAveragePrice *quantity;
@property (nonatomic, strong) QTAveragePrice *status;
@property (nonatomic, strong) QTExchangeOrderID *statusMessage;
@property (nonatomic, strong) QTExchangeOrderID *statusMessageRaw;
@property (nonatomic, strong) QTExchangeOrderID *tag;
@property (nonatomic, strong) QTTags *tags;
@property (nonatomic, strong) QTAveragePrice *tradingsymbol;
@property (nonatomic, strong) QTAveragePrice *transactionType;
@property (nonatomic, strong) QTAveragePrice *triggerPrice;
@property (nonatomic, strong) QTAveragePrice *validity;
@property (nonatomic, strong) QTAveragePrice *validityTTL;
@property (nonatomic, strong) QTAveragePrice *variety;
@end

@interface QTAveragePrice : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTExchangeOrderID : NSObject
@property (nonatomic, copy) NSArray<QTAveragePrice *> *anyOf;
@end

@interface QTExchangeETimestamp : NSObject
@property (nonatomic, copy) NSArray<QTOrderTimestamp *> *anyOf;
@end

@interface QTOrderTimestamp : NSObject
@property (nonatomic, nullable, copy) NSString *format;
@property (nonatomic, assign)         QTType *type;
@end

@interface QTMeta : NSObject
@property (nonatomic, copy) NSString *ref;
@end

@interface QTTags : NSObject
@property (nonatomic, strong) QTAveragePrice *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTIceberg : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTIcebergProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTIcebergProperties : NSObject
@property (nonatomic, strong) QTAveragePrice *leg;
@property (nonatomic, strong) QTAveragePrice *legQuantity;
@property (nonatomic, strong) QTAveragePrice *legs;
@property (nonatomic, strong) QTAveragePrice *remainingQuantity;
@property (nonatomic, strong) QTAveragePrice *totalQuantity;
@end

@interface QTMetaClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTMetaProperties *properties;
@property (nonatomic, copy)   NSArray *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMetaProperties : NSObject
@property (nonatomic, strong) QTMeta *iceberg;
@end

@interface QTOrdersClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTOrdersProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTOrdersProperties : NSObject
@property (nonatomic, strong) QTData *data;
@property (nonatomic, strong) QTAveragePrice *status;
@end

@interface QTData : NSObject
@property (nonatomic, strong) QTMeta *items;
@property (nonatomic, copy)   NSString *type;
@end

NS_ASSUME_NONNULL_END
