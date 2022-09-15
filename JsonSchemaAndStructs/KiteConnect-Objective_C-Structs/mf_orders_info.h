// To parse this JSON:
//
//   NSError *error;
//   QTMFOrdersInfo *mFOrdersInfo = [QTMFOrdersInfo fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTMFOrdersInfo;
@class QTDefinitions;
@class QTData;
@class QTDataProperties;
@class QTAmount;
@class QTType;
@class QTLastPriceDate;
@class QTMFOrdersInfoClass;
@class QTMFOrdersInfoProperties;
@class QTDataClass;

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

@interface QTMFOrdersInfo : NSObject
@property (nonatomic, copy)   NSString *ref;
@property (nonatomic, copy)   NSString *schema;
@property (nonatomic, strong) QTDefinitions *definitions;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface QTDefinitions : NSObject
@property (nonatomic, strong) QTData *data;
@property (nonatomic, strong) QTMFOrdersInfoClass *mfOrdersInfo;
@end

@interface QTData : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDataProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataProperties : NSObject
@property (nonatomic, strong) QTAmount *amount;
@property (nonatomic, strong) QTAmount *averagePrice;
@property (nonatomic, strong) QTAmount *exchangeOrderID;
@property (nonatomic, strong) QTAmount *exchangeTimestamp;
@property (nonatomic, strong) QTAmount *folio;
@property (nonatomic, strong) QTAmount *fund;
@property (nonatomic, strong) QTAmount *lastPrice;
@property (nonatomic, strong) QTLastPriceDate *lastPriceDate;
@property (nonatomic, strong) QTLastPriceDate *orderID;
@property (nonatomic, strong) QTLastPriceDate *orderTimestamp;
@property (nonatomic, strong) QTAmount *placedBy;
@property (nonatomic, strong) QTAmount *purchaseType;
@property (nonatomic, strong) QTAmount *quantity;
@property (nonatomic, strong) QTAmount *settlementID;
@property (nonatomic, strong) QTAmount *status;
@property (nonatomic, strong) QTAmount *statusMessage;
@property (nonatomic, strong) QTAmount *tag;
@property (nonatomic, strong) QTAmount *tradingsymbol;
@property (nonatomic, strong) QTAmount *transactionType;
@property (nonatomic, strong) QTAmount *variety;
@end

@interface QTAmount : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTLastPriceDate : NSObject
@property (nonatomic, copy)   NSString *format;
@property (nonatomic, assign) QTType *type;
@end

@interface QTMFOrdersInfoClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTMFOrdersInfoProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMFOrdersInfoProperties : NSObject
@property (nonatomic, strong) QTDataClass *data;
@property (nonatomic, strong) QTAmount *status;
@end

@interface QTDataClass : NSObject
@property (nonatomic, copy) NSString *ref;
@end

NS_ASSUME_NONNULL_END
