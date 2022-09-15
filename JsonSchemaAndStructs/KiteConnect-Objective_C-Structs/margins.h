// To parse this JSON:
//
//   NSError *error;
//   QTMargins *margins = [QTMargins fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTMargins;
@class QTDefinitions;
@class QTAvailable;
@class QTAvailableProperties;
@class QTAdhocMargin;
@class QTData;
@class QTDataProperties;
@class QTCommodity;
@class QTIty;
@class QTItyProperties;
@class QTUtilised;
@class QTMarginsClass;
@class QTMarginsProperties;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface QTMargins : NSObject
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
@property (nonatomic, strong) QTIty *ity;
@property (nonatomic, strong) QTMarginsClass *margins;
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
@property (nonatomic, strong) QTCommodity *commodity;
@property (nonatomic, strong) QTCommodity *equity;
@end

@interface QTCommodity : NSObject
@property (nonatomic, copy) NSString *ref;
@end

@interface QTIty : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTItyProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTItyProperties : NSObject
@property (nonatomic, strong) QTCommodity *available;
@property (nonatomic, strong) QTAdhocMargin *enabled;
@property (nonatomic, strong) QTAdhocMargin *net;
@property (nonatomic, strong) QTUtilised *utilised;
@end

@interface QTUtilised : NSObject
@property (nonatomic, strong) QTAdhocMargin *additionalProperties;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMarginsClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTMarginsProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMarginsProperties : NSObject
@property (nonatomic, strong) QTCommodity *data;
@property (nonatomic, strong) QTAdhocMargin *status;
@end

NS_ASSUME_NONNULL_END
