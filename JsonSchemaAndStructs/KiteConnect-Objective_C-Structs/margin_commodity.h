// To parse this JSON:
//
//   NSError *error;
//   QTMarginCommodity *marginCommodity = [QTMarginCommodity fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTMarginCommodity;
@class QTDefinitions;
@class QTAvailable;
@class QTAvailableProperties;
@class QTAdhocMargin;
@class QTData;
@class QTDataProperties;
@class QTAvailableClass;
@class QTUtilised;
@class QTMarginCommodityClass;
@class QTMarginCommodityProperties;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface QTMarginCommodity : NSObject
@property (nonatomic, copy)   NSString *ref;
@property (nonatomic, copy)   NSString *schema;
@property (nonatomic, strong) QTDefinitions *definitions;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface QTDefinitions : NSObject
@property (nonatomic, strong) QTAvailable *available;
@property (nonatomic, strong) QTData *data;
@property (nonatomic, strong) QTMarginCommodityClass *marginCommodity;
@end

@interface QTAvailable : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTAvailableProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTAvailableProperties : NSObject
@property (nonatomic, strong) QTAdhocMargin *adhocMargin;
@property (nonatomic, strong) QTAdhocMargin *cash;
@property (nonatomic, strong) QTAdhocMargin *collateral;
@property (nonatomic, strong) QTAdhocMargin *intradayPayin;
@property (nonatomic, strong) QTAdhocMargin *liveBalance;
@property (nonatomic, strong) QTAdhocMargin *openingBalance;
@end

@interface QTAdhocMargin : NSObject
@property (nonatomic, copy) NSString *type;
@end

@interface QTData : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDataProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataProperties : NSObject
@property (nonatomic, strong) QTAvailableClass *available;
@property (nonatomic, strong) QTAdhocMargin *enabled;
@property (nonatomic, strong) QTAdhocMargin *net;
@property (nonatomic, strong) QTUtilised *utilised;
@end

@interface QTAvailableClass : NSObject
@property (nonatomic, copy) NSString *ref;
@end

@interface QTUtilised : NSObject
@property (nonatomic, strong) QTAdhocMargin *additionalProperties;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMarginCommodityClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTMarginCommodityProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMarginCommodityProperties : NSObject
@property (nonatomic, strong) QTAvailableClass *data;
@property (nonatomic, strong) QTAdhocMargin *status;
@end

NS_ASSUME_NONNULL_END
