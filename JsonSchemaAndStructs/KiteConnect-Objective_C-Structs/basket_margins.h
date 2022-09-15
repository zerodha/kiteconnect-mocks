// To parse this JSON:
//
//   NSError *error;
//   QTBasketMargins *basketMargins = [QTBasketMargins fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTBasketMargins;
@class QTDefinitions;
@class QTBasketMarginsClass;
@class QTBasketMarginsProperties;
@class QTData;
@class QTStatus;
@class QTType;
@class QTDataClass;
@class QTDataProperties;
@class QTOrders;
@class QTFinal;
@class QTFinalProperties;
@class QTPnl;
@class QTPnlProperties;

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

@interface QTBasketMargins : NSObject
@property (nonatomic, copy)   NSString *ref;
@property (nonatomic, copy)   NSString *schema;
@property (nonatomic, strong) QTDefinitions *definitions;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface QTDefinitions : NSObject
@property (nonatomic, strong) QTBasketMarginsClass *basketMargins;
@property (nonatomic, strong) QTDataClass *data;
@property (nonatomic, strong) QTFinal *final;
@property (nonatomic, strong) QTPnl *pnl;
@end

@interface QTBasketMarginsClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTBasketMarginsProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTBasketMarginsProperties : NSObject
@property (nonatomic, strong) QTData *data;
@property (nonatomic, strong) QTStatus *status;
@end

@interface QTData : NSObject
@property (nonatomic, copy) NSString *ref;
@end

@interface QTStatus : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTDataClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDataProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataProperties : NSObject
@property (nonatomic, strong) QTData *final;
@property (nonatomic, strong) QTData *initial;
@property (nonatomic, strong) QTOrders *orders;
@end

@interface QTOrders : NSObject
@property (nonatomic, strong) QTData *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTFinal : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTFinalProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTFinalProperties : NSObject
@property (nonatomic, strong) QTStatus *additional;
@property (nonatomic, strong) QTStatus *bo;
@property (nonatomic, strong) QTStatus *cash;
@property (nonatomic, strong) QTStatus *exchange;
@property (nonatomic, strong) QTStatus *exposure;
@property (nonatomic, strong) QTStatus *optionPremium;
@property (nonatomic, strong) QTData *pnl;
@property (nonatomic, strong) QTStatus *span;
@property (nonatomic, strong) QTStatus *total;
@property (nonatomic, strong) QTStatus *tradingsymbol;
@property (nonatomic, strong) QTStatus *type;
@property (nonatomic, strong) QTStatus *var;
@end

@interface QTPnl : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTPnlProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTPnlProperties : NSObject
@property (nonatomic, strong) QTStatus *realised;
@property (nonatomic, strong) QTStatus *unrealised;
@end

NS_ASSUME_NONNULL_END
