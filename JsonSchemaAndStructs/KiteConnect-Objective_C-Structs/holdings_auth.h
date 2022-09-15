// To parse this JSON:
//
//   NSError *error;
//   QTHoldingsAuth *holdingsAuth = [QTHoldingsAuth fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTHoldingsAuth;
@class QTDefinitions;
@class QTData;
@class QTDataProperties;
@class QTRequestID;
@class QTHoldingsAuthClass;
@class QTHoldingsAuthProperties;
@class QTDataClass;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface QTHoldingsAuth : NSObject
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
@property (nonatomic, strong) QTHoldingsAuthClass *holdingsAuth;
@end

@interface QTData : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDataProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataProperties : NSObject
@property (nonatomic, strong) QTRequestID *requestID;
@end

@interface QTRequestID : NSObject
@property (nonatomic, copy) NSString *type;
@end

@interface QTHoldingsAuthClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTHoldingsAuthProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTHoldingsAuthProperties : NSObject
@property (nonatomic, strong) QTDataClass *data;
@property (nonatomic, strong) QTRequestID *status;
@end

@interface QTDataClass : NSObject
@property (nonatomic, copy) NSString *ref;
@end

NS_ASSUME_NONNULL_END
