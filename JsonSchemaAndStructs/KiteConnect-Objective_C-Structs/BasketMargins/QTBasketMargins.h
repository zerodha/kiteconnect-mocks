// To parse this JSON:
//
//   NSError *error;
//   QTBasketMargins *basketMargins = [QTBasketMargins fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTBasketMargins;
@class QTData;
@class QTFinal;
@class QTPnl;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface QTBasketMargins : NSObject
@property (nonatomic, nullable, strong) QTData *data;
@property (nonatomic, nullable, copy)   NSString *status;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface QTData : NSObject
@property (nonatomic, nullable, strong) QTFinal *final;
@property (nonatomic, nullable, strong) QTFinal *initial;
@property (nonatomic, nullable, copy)   NSArray<QTFinal *> *orders;
@end

@interface QTFinal : NSObject
@property (nonatomic, nullable, strong) NSNumber *additional;
@property (nonatomic, nullable, strong) NSNumber *bo;
@property (nonatomic, nullable, strong) NSNumber *cash;
@property (nonatomic, nullable, copy)   NSString *exchange;
@property (nonatomic, nullable, strong) NSNumber *exposure;
@property (nonatomic, nullable, strong) NSNumber *optionPremium;
@property (nonatomic, nullable, strong) QTPnl *pnl;
@property (nonatomic, nullable, strong) NSNumber *span;
@property (nonatomic, nullable, strong) NSNumber *total;
@property (nonatomic, nullable, copy)   NSString *tradingsymbol;
@property (nonatomic, nullable, copy)   NSString *type;
@property (nonatomic, nullable, strong) NSNumber *var;
@end

@interface QTPnl : NSObject
@property (nonatomic, nullable, strong) NSNumber *realised;
@property (nonatomic, nullable, strong) NSNumber *unrealised;
@end

NS_ASSUME_NONNULL_END
