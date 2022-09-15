// To parse this JSON:
//
//   NSError *error;
//   QTHoldings *holdings = [QTHoldings fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTHoldings;
@class QTDefinitions;
@class QTDatum;
@class QTDatumProperties;
@class QTAuthorisedDate;
@class QTType;
@class QTAuthorisedQuantity;
@class QTHoldingsClass;
@class QTHoldingsProperties;
@class QTData;
@class QTItems;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Boxed enums

@interface QTType : NSObject
@property (nonatomic, readonly, copy) NSString *value;
+ (instancetype _Nullable)withValue:(NSString *)value;
+ (QTType *)boolean;
+ (QTType *)integer;
+ (QTType *)number;
+ (QTType *)string;
@end

#pragma mark - Object interfaces

@interface QTHoldings : NSObject
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
@property (nonatomic, strong) QTHoldingsClass *holdings;
@end

@interface QTDatum : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDatumProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDatumProperties : NSObject
@property (nonatomic, strong) QTAuthorisedDate *authorisedDate;
@property (nonatomic, strong) QTAuthorisedQuantity *authorisedQuantity;
@property (nonatomic, strong) QTAuthorisedQuantity *averagePrice;
@property (nonatomic, strong) QTAuthorisedQuantity *closePrice;
@property (nonatomic, strong) QTAuthorisedQuantity *collateralQuantity;
@property (nonatomic, strong) QTAuthorisedQuantity *collateralType;
@property (nonatomic, strong) QTAuthorisedQuantity *dayChange;
@property (nonatomic, strong) QTAuthorisedQuantity *dayChangePercentage;
@property (nonatomic, strong) QTAuthorisedQuantity *discrepancy;
@property (nonatomic, strong) QTAuthorisedQuantity *exchange;
@property (nonatomic, strong) QTAuthorisedQuantity *instrumentToken;
@property (nonatomic, strong) QTAuthorisedQuantity *isin;
@property (nonatomic, strong) QTAuthorisedQuantity *lastPrice;
@property (nonatomic, strong) QTAuthorisedQuantity *openingQuantity;
@property (nonatomic, strong) QTAuthorisedQuantity *pnl;
@property (nonatomic, strong) QTAuthorisedQuantity *price;
@property (nonatomic, strong) QTAuthorisedQuantity *product;
@property (nonatomic, strong) QTAuthorisedQuantity *quantity;
@property (nonatomic, strong) QTAuthorisedQuantity *realisedQuantity;
@property (nonatomic, strong) QTAuthorisedQuantity *t1Quantity;
@property (nonatomic, strong) QTAuthorisedQuantity *tradingsymbol;
@property (nonatomic, strong) QTAuthorisedQuantity *usedQuantity;
@end

@interface QTAuthorisedDate : NSObject
@property (nonatomic, copy)   NSString *format;
@property (nonatomic, assign) QTType *type;
@end

@interface QTAuthorisedQuantity : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTHoldingsClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTHoldingsProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTHoldingsProperties : NSObject
@property (nonatomic, strong) QTData *data;
@property (nonatomic, strong) QTAuthorisedQuantity *status;
@end

@interface QTData : NSObject
@property (nonatomic, strong) QTItems *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTItems : NSObject
@property (nonatomic, copy) NSString *ref;
@end

NS_ASSUME_NONNULL_END
