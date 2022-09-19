// To parse this JSON:
//
//   NSError *error;
//   Holdings * = [Holdings fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class Holdings;
@class HoldingsDatum;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface Holdings : NSObject
@property (nonatomic, nullable, copy) NSArray<HoldingsDatum *> *data;
@property (nonatomic, nullable, copy) NSString *status;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface HoldingsDatum : NSObject
@property (nonatomic, nullable, copy)   NSString *authorisedDate;
@property (nonatomic, nullable, strong) NSNumber *authorisedQuantity;
@property (nonatomic, nullable, strong) NSNumber *averagePrice;
@property (nonatomic, nullable, strong) NSNumber *closePrice;
@property (nonatomic, nullable, strong) NSNumber *collateralQuantity;
@property (nonatomic, nullable, copy)   NSString *collateralType;
@property (nonatomic, nullable, strong) NSNumber *dayChange;
@property (nonatomic, nullable, strong) NSNumber *dayChangePercentage;
@property (nonatomic, nullable, strong) NSNumber *discrepancy;
@property (nonatomic, nullable, copy)   NSString *exchange;
@property (nonatomic, nullable, strong) NSNumber *instrumentToken;
@property (nonatomic, nullable, copy)   NSString *isin;
@property (nonatomic, nullable, strong) NSNumber *lastPrice;
@property (nonatomic, nullable, strong) NSNumber *openingQuantity;
@property (nonatomic, nullable, strong) NSNumber *pnl;
@property (nonatomic, nullable, strong) NSNumber *price;
@property (nonatomic, nullable, copy)   NSString *product;
@property (nonatomic, nullable, strong) NSNumber *quantity;
@property (nonatomic, nullable, strong) NSNumber *realisedQuantity;
@property (nonatomic, nullable, strong) NSNumber *t1Quantity;
@property (nonatomic, nullable, copy)   NSString *tradingsymbol;
@property (nonatomic, nullable, strong) NSNumber *usedQuantity;
@end

NS_ASSUME_NONNULL_END
