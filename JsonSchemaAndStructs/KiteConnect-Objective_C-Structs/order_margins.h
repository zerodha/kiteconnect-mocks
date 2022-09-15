// To parse this JSON:
//
//   NSError *error;
//   QTOrderMargins *orderMargins = [QTOrderMargins fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTOrderMargins;
@class QTDefinitions;
@class QTDatum;
@class QTDatumProperties;
@class QTAdditional;
@class QTType;
@class QTPnl;
@class QTOrderMarginsClass;
@class QTOrderMarginsProperties;
@class QTData;
@class QTPnlClass;
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

@interface QTOrderMargins : NSObject
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
@property (nonatomic, strong) QTOrderMarginsClass *orderMargins;
@property (nonatomic, strong) QTPnlClass *pnl;
@end

@interface QTDatum : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDatumProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDatumProperties : NSObject
@property (nonatomic, strong) QTAdditional *additional;
@property (nonatomic, strong) QTAdditional *bo;
@property (nonatomic, strong) QTAdditional *cash;
@property (nonatomic, strong) QTAdditional *exchange;
@property (nonatomic, strong) QTAdditional *exposure;
@property (nonatomic, strong) QTAdditional *optionPremium;
@property (nonatomic, strong) QTPnl *pnl;
@property (nonatomic, strong) QTAdditional *span;
@property (nonatomic, strong) QTAdditional *total;
@property (nonatomic, strong) QTAdditional *tradingsymbol;
@property (nonatomic, strong) QTAdditional *type;
@property (nonatomic, strong) QTAdditional *var;
@end

@interface QTAdditional : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTPnl : NSObject
@property (nonatomic, copy) NSString *ref;
@end

@interface QTOrderMarginsClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTOrderMarginsProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTOrderMarginsProperties : NSObject
@property (nonatomic, strong) QTData *data;
@property (nonatomic, strong) QTAdditional *status;
@end

@interface QTData : NSObject
@property (nonatomic, strong) QTPnl *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTPnlClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTPnlProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTPnlProperties : NSObject
@property (nonatomic, strong) QTAdditional *realised;
@property (nonatomic, strong) QTAdditional *unrealised;
@end

NS_ASSUME_NONNULL_END
