// To parse this JSON:
//
//   NSError *error;
//   QTLtp *ltp = [QTLtp fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTLtp;
@class QTDefinitions;
@class QTData;
@class QTDataProperties;
@class QTNseInfy;
@class QTLtpClass;
@class QTLtpProperties;
@class QTStatus;
@class QTNseInfyClass;
@class QTNseInfyProperties;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface QTLtp : NSObject
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
@property (nonatomic, strong) QTLtpClass *ltp;
@property (nonatomic, strong) QTNseInfyClass *nseInfy;
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

@interface QTLtpClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTLtpProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTLtpProperties : NSObject
@property (nonatomic, strong) QTNseInfy *data;
@property (nonatomic, strong) QTStatus *status;
@end

@interface QTStatus : NSObject
@property (nonatomic, copy) NSString *type;
@end

@interface QTNseInfyClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTNseInfyProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTNseInfyProperties : NSObject
@property (nonatomic, strong) QTStatus *instrumentToken;
@property (nonatomic, strong) QTStatus *lastPrice;
@end

NS_ASSUME_NONNULL_END
