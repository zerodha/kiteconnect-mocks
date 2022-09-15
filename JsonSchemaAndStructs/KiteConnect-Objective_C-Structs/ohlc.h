// To parse this JSON:
//
//   NSError *error;
//   QTOhlc *ohlc = [QTOhlc fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTOhlc;
@class QTDefinitions;
@class QTData;
@class QTDataProperties;
@class QTNseInfy;
@class QTNseInfyClass;
@class QTNseInfyProperties;
@class QTInstrumentToken;
@class QTOhlcClass;
@class QTOhlcProperties;
@class QTOhlcClassClass;
@class QTOhlcClassProperties;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface QTOhlc : NSObject
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
@property (nonatomic, strong) QTNseInfyClass *nseInfy;
@property (nonatomic, strong) QTOhlcClass *ohlc;
@property (nonatomic, strong) QTOhlcClassClass *ohlcClass;
@end

@interface QTData : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDataProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataProperties : NSObject
@property (nonatomic, strong) QTNseInfy *nseInfy;
@end

@interface QTNseInfy : NSObject
@property (nonatomic, copy) NSString *ref;
@end

@interface QTNseInfyClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTNseInfyProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTNseInfyProperties : NSObject
@property (nonatomic, strong) QTInstrumentToken *instrumentToken;
@property (nonatomic, strong) QTInstrumentToken *lastPrice;
@property (nonatomic, strong) QTNseInfy *ohlc;
@end

@interface QTInstrumentToken : NSObject
@property (nonatomic, copy) NSString *type;
@end

@interface QTOhlcClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTOhlcProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTOhlcProperties : NSObject
@property (nonatomic, strong) QTNseInfy *data;
@property (nonatomic, strong) QTInstrumentToken *status;
@end

@interface QTOhlcClassClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTOhlcClassProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTOhlcClassProperties : NSObject
@property (nonatomic, strong) QTInstrumentToken *close;
@property (nonatomic, strong) QTInstrumentToken *high;
@property (nonatomic, strong) QTInstrumentToken *low;
@property (nonatomic, strong) QTInstrumentToken *open;
@end

NS_ASSUME_NONNULL_END
