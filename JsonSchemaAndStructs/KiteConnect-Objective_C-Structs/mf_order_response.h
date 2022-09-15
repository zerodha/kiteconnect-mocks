// To parse this JSON:
//
//   NSError *error;
//   QTMFOrderResponse *mFOrderResponse = [QTMFOrderResponse fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTMFOrderResponse;
@class QTDefinitions;
@class QTData;
@class QTDataProperties;
@class QTOrderID;
@class QTMFOrderResponseClass;
@class QTMFOrderResponseProperties;
@class QTDataClass;
@class QTStatus;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface QTMFOrderResponse : NSObject
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
@property (nonatomic, strong) QTMFOrderResponseClass *mfOrderResponse;
@end

@interface QTData : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDataProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataProperties : NSObject
@property (nonatomic, strong) QTOrderID *orderID;
@end

@interface QTOrderID : NSObject
@property (nonatomic, copy) NSString *format;
@property (nonatomic, copy) NSString *type;
@end

@interface QTMFOrderResponseClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTMFOrderResponseProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMFOrderResponseProperties : NSObject
@property (nonatomic, strong) QTDataClass *data;
@property (nonatomic, strong) QTStatus *status;
@end

@interface QTDataClass : NSObject
@property (nonatomic, copy) NSString *ref;
@end

@interface QTStatus : NSObject
@property (nonatomic, copy) NSString *type;
@end

NS_ASSUME_NONNULL_END
