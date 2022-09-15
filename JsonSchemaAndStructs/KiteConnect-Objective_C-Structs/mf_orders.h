// To parse this JSON:
//
//   NSError *error;
//   QTMFOrders *mFOrders = [QTMFOrders fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTMFOrders;
@class QTDefinitions;
@class QTDatum;
@class QTDatumProperties;
@class QTAmount;
@class QTType;
@class QTExchangeOrderID;
@class QTLastPriceDate;
@class QTTag;
@class QTMFOrdersClass;
@class QTMFOrdersProperties;
@class QTData;
@class QTItems;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Boxed enums

@interface QTType : NSObject
@property (nonatomic, readonly, copy) NSString *value;
+ (instancetype _Nullable)withValue:(NSString *)value;
+ (QTType *)null;
+ (QTType *)number;
+ (QTType *)string;
@end

#pragma mark - Object interfaces

@interface QTMFOrders : NSObject
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
@property (nonatomic, strong) QTMFOrdersClass *mfOrders;
@end

@interface QTDatum : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDatumProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDatumProperties : NSObject
@property (nonatomic, strong) QTAmount *amount;
@property (nonatomic, strong) QTAmount *averagePrice;
@property (nonatomic, strong) QTExchangeOrderID *exchangeOrderID;
@property (nonatomic, strong) QTExchangeOrderID *exchangeTimestamp;
@property (nonatomic, strong) QTAmount *folio;
@property (nonatomic, strong) QTAmount *fund;
@property (nonatomic, strong) QTAmount *lastPrice;
@property (nonatomic, strong) QTLastPriceDate *lastPriceDate;
@property (nonatomic, strong) QTLastPriceDate *orderID;
@property (nonatomic, strong) QTLastPriceDate *orderTimestamp;
@property (nonatomic, strong) QTAmount *placedBy;
@property (nonatomic, strong) QTAmount *purchaseType;
@property (nonatomic, strong) QTAmount *quantity;
@property (nonatomic, strong) QTExchangeOrderID *settlementID;
@property (nonatomic, strong) QTAmount *status;
@property (nonatomic, strong) QTAmount *statusMessage;
@property (nonatomic, strong) QTTag *tag;
@property (nonatomic, strong) QTAmount *tradingsymbol;
@property (nonatomic, strong) QTAmount *transactionType;
@property (nonatomic, strong) QTAmount *variety;
@end

@interface QTAmount : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTExchangeOrderID : NSObject
@property (nonatomic, copy) NSArray<QTLastPriceDate *> *anyOf;
@end

@interface QTLastPriceDate : NSObject
@property (nonatomic, nullable, copy) NSString *format;
@property (nonatomic, assign)         QTType *type;
@end

@interface QTTag : NSObject
@property (nonatomic, copy) NSArray<QTAmount *> *anyOf;
@end

@interface QTMFOrdersClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTMFOrdersProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMFOrdersProperties : NSObject
@property (nonatomic, strong) QTData *data;
@property (nonatomic, strong) QTAmount *status;
@end

@interface QTData : NSObject
@property (nonatomic, strong) QTItems *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTItems : NSObject
@property (nonatomic, copy) NSString *ref;
@end

NS_ASSUME_NONNULL_END
