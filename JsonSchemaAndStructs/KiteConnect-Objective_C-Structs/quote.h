// To parse this JSON:
//
//   NSError *error;
//   QTQuote *quote = [QTQuote fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTQuote;
@class QTDefinitions;
@class QTBuy;
@class QTBuyProperties;
@class QTOrders;
@class QTType;
@class QTData;
@class QTDataProperties;
@class QTNseInfy;
@class QTDepth;
@class QTDepthProperties;
@class QTBuyClass;
@class QTNseInfyClass;
@class QTNseInfyProperties;
@class QTLastTradeTime;
@class QTOhlc;
@class QTOhlcProperties;
@class QTQuoteClass;
@class QTQuoteProperties;

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

@interface QTQuote : NSObject
@property (nonatomic, copy)   NSString *ref;
@property (nonatomic, copy)   NSString *schema;
@property (nonatomic, strong) QTDefinitions *definitions;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface QTDefinitions : NSObject
@property (nonatomic, strong) QTBuy *buy;
@property (nonatomic, strong) QTData *data;
@property (nonatomic, strong) QTDepth *depth;
@property (nonatomic, strong) QTNseInfyClass *nseInfy;
@property (nonatomic, strong) QTOhlc *ohlc;
@property (nonatomic, strong) QTQuoteClass *quote;
@end

@interface QTBuy : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTBuyProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTBuyProperties : NSObject
@property (nonatomic, strong) QTOrders *orders;
@property (nonatomic, strong) QTOrders *price;
@property (nonatomic, strong) QTOrders *quantity;
@end

@interface QTOrders : NSObject
@property (nonatomic, assign) QTType *type;
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

@interface QTDepth : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDepthProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDepthProperties : NSObject
@property (nonatomic, strong) QTBuyClass *buy;
@property (nonatomic, strong) QTBuyClass *sell;
@end

@interface QTBuyClass : NSObject
@property (nonatomic, strong) QTNseInfy *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTNseInfyClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTNseInfyProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTNseInfyProperties : NSObject
@property (nonatomic, strong) QTOrders *averagePrice;
@property (nonatomic, strong) QTOrders *buyQuantity;
@property (nonatomic, strong) QTNseInfy *depth;
@property (nonatomic, strong) QTOrders *instrumentToken;
@property (nonatomic, strong) QTOrders *lastPrice;
@property (nonatomic, strong) QTOrders *lastQuantity;
@property (nonatomic, strong) QTLastTradeTime *lastTradeTime;
@property (nonatomic, strong) QTOrders *lowerCircuitLimit;
@property (nonatomic, strong) QTOrders *netChange;
@property (nonatomic, strong) QTNseInfy *ohlc;
@property (nonatomic, strong) QTOrders *oi;
@property (nonatomic, strong) QTOrders *oiDayHigh;
@property (nonatomic, strong) QTOrders *oiDayLow;
@property (nonatomic, strong) QTOrders *sellQuantity;
@property (nonatomic, strong) QTLastTradeTime *timestamp;
@property (nonatomic, strong) QTOrders *upperCircuitLimit;
@property (nonatomic, strong) QTOrders *volume;
@end

@interface QTLastTradeTime : NSObject
@property (nonatomic, copy)   NSString *format;
@property (nonatomic, assign) QTType *type;
@end

@interface QTOhlc : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTOhlcProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTOhlcProperties : NSObject
@property (nonatomic, strong) QTOrders *close;
@property (nonatomic, strong) QTOrders *high;
@property (nonatomic, strong) QTOrders *low;
@property (nonatomic, strong) QTOrders *open;
@end

@interface QTQuoteClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTQuoteProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTQuoteProperties : NSObject
@property (nonatomic, strong) QTNseInfy *data;
@property (nonatomic, strong) QTOrders *status;
@end

NS_ASSUME_NONNULL_END
