// To parse this JSON:
//
//   NSError *error;
//   QTMFHoldings *mFHoldings = [QTMFHoldings fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTMFHoldings;
@class QTDefinitions;
@class QTDatum;
@class QTDatumProperties;
@class QTAveragePrice;
@class QTType;
@class QTMFHoldingsClass;
@class QTMFHoldingsProperties;
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

@interface QTMFHoldings : NSObject
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
@property (nonatomic, strong) QTMFHoldingsClass *mfHoldings;
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
@property (nonatomic, strong) QTAveragePrice *folio;
@property (nonatomic, strong) QTAveragePrice *fund;
@property (nonatomic, strong) QTAveragePrice *lastPrice;
@property (nonatomic, strong) QTAveragePrice *lastPriceDate;
@property (nonatomic, strong) QTAveragePrice *pledgedQuantity;
@property (nonatomic, strong) QTAveragePrice *pnl;
@property (nonatomic, strong) QTAveragePrice *quantity;
@property (nonatomic, strong) QTAveragePrice *tradingsymbol;
@end

@interface QTAveragePrice : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTMFHoldingsClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTMFHoldingsProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMFHoldingsProperties : NSObject
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
