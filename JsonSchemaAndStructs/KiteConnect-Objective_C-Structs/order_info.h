// To parse this JSON:
//
//   NSError *error;
//   QTOrderInfo *orderInfo = [QTOrderInfo fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTOrderInfo;
@class QTDefinitions;
@class QTDatum;
@class QTDatumProperties;
@class QTAveragePrice;
@class QTType;
@class QTExchangeOrderID;
@class QTExchangeTimestamp;
@class QTOrderTimestamp;
@class QTOrderInfoClass;
@class QTOrderInfoProperties;
@class QTData;
@class QTItems;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Boxed enums

@interface QTType : NSObject
@property (nonatomic, readonly, copy) NSString *value;
+ (instancetype _Nullable)withValue:(NSString *)value;
+ (QTType *)integer;
+ (QTType *)null;
+ (QTType *)number;
+ (QTType *)string;
@end

#pragma mark - Object interfaces

@interface QTOrderInfo : NSObject
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
@property (nonatomic, strong) QTOrderInfoClass *orderInfo;
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
@property (nonatomic, strong) QTExchangeTimestamp *exchangeTimestamp;
@property (nonatomic, strong) QTAveragePrice *filledQuantity;
@property (nonatomic, strong) QTAveragePrice *instrumentToken;
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
@property (nonatomic, strong) QTAveragePrice *statusMessage;
@property (nonatomic, strong) QTAveragePrice *tag;
@property (nonatomic, strong) QTAveragePrice *tradingsymbol;
@property (nonatomic, strong) QTAveragePrice *transactionType;
@property (nonatomic, strong) QTAveragePrice *triggerPrice;
@property (nonatomic, strong) QTAveragePrice *validity;
@property (nonatomic, strong) QTAveragePrice *variety;
@end

@interface QTAveragePrice : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTExchangeOrderID : NSObject
@property (nonatomic, copy) NSArray<QTAveragePrice *> *anyOf;
@end

@interface QTExchangeTimestamp : NSObject
@property (nonatomic, copy) NSArray<QTOrderTimestamp *> *anyOf;
@end

@interface QTOrderTimestamp : NSObject
@property (nonatomic, nullable, copy) NSString *format;
@property (nonatomic, assign)         QTType *type;
@end

@interface QTOrderInfoClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTOrderInfoProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTOrderInfoProperties : NSObject
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
