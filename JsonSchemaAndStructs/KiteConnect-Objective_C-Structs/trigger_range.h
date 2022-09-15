// To parse this JSON:
//
//   NSError *error;
//   QTTriggerRange *triggerRange = [QTTriggerRange fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTTriggerRange;
@class QTDefinitions;
@class QTData;
@class QTDataProperties;
@class QTNseInfy;
@class QTNse;
@class QTNseProperties;
@class QTInstrumentToken;
@class QTTriggerRangeClass;
@class QTTriggerRangeProperties;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface QTTriggerRange : NSObject
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
@property (nonatomic, strong) QTNse *nse;
@property (nonatomic, strong) QTTriggerRangeClass *triggerRange;
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
@property (nonatomic, strong) QTNseInfy *nseReliance;
@end

@interface QTNseInfy : NSObject
@property (nonatomic, copy) NSString *ref;
@end

@interface QTNse : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTNseProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTNseProperties : NSObject
@property (nonatomic, strong) QTInstrumentToken *instrumentToken;
@property (nonatomic, strong) QTInstrumentToken *lower;
@property (nonatomic, strong) QTInstrumentToken *upper;
@end

@interface QTInstrumentToken : NSObject
@property (nonatomic, copy) NSString *type;
@end

@interface QTTriggerRangeClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTTriggerRangeProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTTriggerRangeProperties : NSObject
@property (nonatomic, strong) QTNseInfy *data;
@property (nonatomic, strong) QTInstrumentToken *status;
@end

NS_ASSUME_NONNULL_END
